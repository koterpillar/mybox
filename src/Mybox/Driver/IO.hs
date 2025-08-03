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

data IODriver = IODriver
  { transformArgs :: Args -> Args
  , cwd :: Maybe Path
  }

runDriverIO :: IOE :> es => IODriver -> Eff (Driver : es) a -> Eff es a
runDriverIO drv =
  interpret_ $ \case
    DrvRun exitBehavior outputBehavior args_ -> do
      let (cmd :| args) = Text.unpack <$> drv.transformArgs args_
      let process = (proc cmd args){System.Process.cwd = Text.unpack . (.text) <$> drv.cwd}
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
localDriver = runDriverIO $ IODriver{transformArgs = id, cwd = Nothing}

withEnv :: IOE :> es => Text -> (Text -> Text) -> Eff es a -> Eff es a
withEnv key fn action = do
  originalValue <- fromMaybe "" <$> localDriver (drvEnv key)
  let newValue = fn originalValue
  bracket_
    (liftIO $ setEnv (Text.unpack key) $ Text.unpack newValue)
    (liftIO $ setEnv (Text.unpack key) $ Text.unpack originalValue)
    action

withAddedPath :: IOE :> es => Path -> Eff es a -> Eff es a
withAddedPath addPath = withEnv "PATH" $ \originalPath -> addPath.text <> ":" <> originalPath

testHostDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
testHostDriver act = do
  githubToken <- localDriver drvGithubToken
  originalHome <- localDriver drvHome
  bracket (localDriver $ drvTemp_ True) (localDriver . drvRm) $ \home -> do
    withAddedPath (home </> ".local" </> "bin") $
      withEnv "GITHUB_TOKEN" (const githubToken) $ do
        localDriver $ do
          let linkToOriginalHome path = do
                let op = originalHome </> path
                let np = home </> path
                drvMkdir op
                drvLink op np
          linkedDirectories <-
            drvOS >>= \case
              Linux _ -> pure [mkPath ".local/share/fonts", mkPath ".local/share/systemd/user"]
              MacOS -> pure [mkPath "Library/Fonts", mkPath "Library/LaunchAgents"]
          for_ linkedDirectories linkToOriginalHome
        let transformArgs ("sh" :| ["-c", "eval echo '~'"]) = "echo" :| [home.text]
            transformArgs args = args
        let cwd = Just home
        flip runDriverIO act $ IODriver{..}

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
        drvCopy "bootstrap" (tempDir </> "bootstrap")
        drvWriteFile (tempDir </> "Dockerfile") $ dockerfile baseImage
        let image = dockerImagePrefix <> baseImage
        drvRun $ "docker" :| ["build", "--tag", image, tempDir.text]
        -- FIXME: --volume for package root
        drvRunOutput $
          "docker"
            :| [ "run"
               , "--rm"
               , "--detach"
               , "--name"
               , "mybox-test-" <> containerName
               , "--env"
               , "GITHUB_TOKEN=" <> githubToken
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
    cwd = Nothing
    transformArgs :: Args -> Args
    transformArgs ("sudo" :| args') = dockerExec "root" args'
    transformArgs args = dockerExec dockerUser $ toList args
    dockerExec :: Text -> [Text] -> Args
    dockerExec user args =
      "docker"
        :| [ "exec"
           , "--user"
           , user
           , "--workdir"
           , (homeOf user).text
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
    , "COPY bootstrap /bootstrap"
    , "RUN /bootstrap --development --haskell"
    , "ENV PATH=" <> (homeOf dockerUser).text <> "/.local/bin:$PATH"
    , "USER " <> dockerUser
    , -- populate dnf cache so each test doesn't have to do it
      "RUN command -v dnf >/dev/null && dnf check-update || true"
    ]

dockerUser :: Text
dockerUser = "regular_user"

homeOf :: Text -> Path
homeOf "root" = pRoot </> "root"
homeOf user = pRoot </> "home" </> pSegment user

dockerImagePrefix :: Text
dockerImagePrefix = "mybox-test-"

testDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
testDriver act =
  localDriver (drvEnv "DOCKER_IMAGE") >>= \case
    Nothing -> testHostDriver act
    Just image -> dockerDriver image act
