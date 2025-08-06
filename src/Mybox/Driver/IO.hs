module Mybox.Driver.IO (
  localDriver,
  testDriver,
) where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import System.Environment
import System.Process
import System.Random

import Mybox.Driver.Class
import Mybox.Driver.Ops
import Mybox.Driver.Platform
import Mybox.Prelude

newtype IODriver = IODriver
  { transformArgs :: Args -> Args
  }

localIODriver :: IODriver
localIODriver = IODriver{transformArgs = id}

runDriverIO :: IOE :> es => IODriver -> Eff (Driver : es) a -> Eff es a
runDriverIO drv =
  interpret_ $ \case
    DrvRun exitBehavior outputBehavior args_ -> do
      let (cmd :| args) = Text.unpack <$> drv.transformArgs args_
      let process = proc cmd args
      (exitCode, stdoutStr, stderrStr) <-
        liftIO $ readCreateProcessWithExitCode process ""
      let stdout = Text.pack stdoutStr
      let stderr = Text.pack stderrStr
      exit <-
        case exitBehavior of
          RunExitError ->
            case exitCode of
              ExitSuccess -> pure ()
              ExitFailure code ->
                terror $
                  ("Process " <> shellJoin args_ <> " failed with exit code: " <> Text.pack (show code))
                    & (if Text.null stderr then id else (<> " and stderr: " <> stderr))
                    & ( case Text.stripPrefix "OCI runtime exec failed: exec failed: " stdout of
                          Nothing -> id
                          Just outMsg -> (<> " and stdout: " <> outMsg)
                      )
          RunExitReturn -> pure exitCode
      output <-
        case outputBehavior of
          RunOutputShow -> void $ liftIO $ Text.putStr stdout
          RunOutputHide -> pure ()
          RunOutputReturn -> pure $ Text.strip stdout
      pure $ RunResult{..}

localDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
localDriver = runDriverIO localIODriver

withEnv :: IOE :> es => Text -> (Text -> Text) -> Eff es a -> Eff es a
withEnv key fn action = do
  originalValue <- fromMaybe "" <$> localDriver (drvEnv key)
  let newValue = fn originalValue
  bracket_
    (liftIO $ setEnv (Text.unpack key) $ Text.unpack newValue)
    (liftIO $ setEnv (Text.unpack key) $ Text.unpack originalValue)
    action

withAddedPath :: IOE :> es => Path Abs -> Eff es a -> Eff es a
withAddedPath addPath = withEnv "PATH" $ \originalPath -> addPath.text <> ":" <> originalPath

testHostDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
testHostDriver act = do
  githubToken <- localDriver drvGithubToken
  originalHome <- localDriver drvHome
  bracket (localDriver $ drvTemp_ True) (localDriver . drvRm) $ \home -> do
    withAddedPath (home </> ".local" </> "bin") $
      withEnv "HOME" (const home.text) $
        withEnv "GITHUB_TOKEN" (const githubToken) $ do
          localDriver $ do
            let linkToOriginalHome :: Driver :> es => Path Rel -> Eff es ()
                linkToOriginalHome path = do
                  let op = originalHome <//> path
                  let np = home <//> path
                  drvMkdir op
                  drvLink op np
            linkedDirectories <-
              drvOS >>= \case
                Linux _ -> pure [mkPath ".local/share/fonts", mkPath ".local/share/systemd/user"]
                MacOS -> pure [mkPath "Library/Fonts", mkPath "Library/LaunchAgents"]
            for_ linkedDirectories linkToOriginalHome
          runDriverIO localIODriver act

dockerDriver :: IOE :> es => Text -> Eff (Driver : es) a -> Eff es a
dockerDriver baseImage act =
  bracket mkContainer rmContainer (flip runDriverIO act . mkDriver)
 where
  mkContainer :: IOE :> es => Eff es Text
  mkContainer =
    localDriver $ do
      containerName <- Text.pack . show <$> liftIO (randomIO @Word)
      githubToken <- drvGithubToken
      drvTempDir $ \tempDir -> do
        drvCopy (pSegment "bootstrap") (tempDir </> "bootstrap")
        drvWriteFile (tempDir </> "Dockerfile") $ dockerfile baseImage
        let image = dockerImagePrefix <> baseImage
        drvRun $ "docker" :| ["build", "--tag", image, tempDir.text]
        drvRunOutput $
          "docker"
            :| [ "run"
               , "--rm"
               , "--detach"
               , "--name"
               , "mybox-test-" <> containerName
               , "--env"
               , "GITHUB_TOKEN=" <> githubToken
               , "--volume"
               , ".:/mybox"
               , image
               , "sleep"
               , "86400000"
               ]
  rmContainer :: IOE :> es => Text -> Eff es ()
  rmContainer container =
    localDriver $ drvRunSilent $ "docker" :| ["rm", "--force", container]
  mkDriver :: Text -> IODriver
  mkDriver container = IODriver{..}
   where
    transformArgs :: Args -> Args
    transformArgs ("sudo" :| args') = dockerExec "root" args'
    transformArgs args = dockerExec dockerUser $ toList args
    dockerExec :: Text -> [Text] -> Args
    dockerExec user args =
      "docker"
        :| [ "exec"
           , "--user"
           , user
           , "--interactive"
           , container
           ]
        <> args

dockerfile ::
  -- | base image
  Text ->
  Text
dockerfile baseImage =
  Text.unlines
    [ "FROM " <> baseImage
    , "RUN useradd --create-home --password '' " <> dockerUser
    , "RUN mkdir /mybox"
    , "WORKDIR /mybox"
    , "COPY bootstrap /bootstrap"
    , "RUN /bootstrap --development --haskell"
    , "ENV PATH=" <> (homeOf dockerUser).text <> "/.local/bin:$PATH"
    , "USER " <> dockerUser
    , -- populate dnf cache so each test doesn't have to do it
      "RUN command -v dnf >/dev/null && dnf check-update || true"
    ]

dockerUser :: Text
dockerUser = "regular_user"

homeOf :: Text -> Path Abs
homeOf "root" = pRoot </> "root"
homeOf user = pRoot </> "home" </> user

dockerImagePrefix :: Text
dockerImagePrefix = "mybox-test-"

testDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
testDriver act =
  localDriver (drvEnv "DOCKER_IMAGE") >>= \case
    Nothing -> testHostDriver act
    Just image -> dockerDriver image act
