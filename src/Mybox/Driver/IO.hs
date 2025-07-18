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
import Mybox.Prelude

data IODriver = IODriver
  { transformArgs :: Args -> Args
  , cwd :: Maybe Text
  }

runDriverIO :: IOE :> es => IODriver -> Eff (Driver : es) a -> Eff es a
runDriverIO drv =
  interpret_ $ \case
    DrvRun exitBehavior outputBehavior args_ -> do
      let (cmd :| args) = Text.unpack <$> drv.transformArgs args_
      let process = (proc cmd args){System.Process.cwd = Text.unpack <$> drv.cwd}
      (exitCode, stdout, stderr) <-
        liftIO $ readCreateProcessWithExitCode process ""
      let stdoutText = Text.pack stdout
      exit <-
        case exitBehavior of
          RunExitError ->
            case exitCode of
              ExitSuccess -> pure ()
              ExitFailure code ->
                terror $
                  "Process "
                    <> shellJoin args_
                    <> " failed with exit code: "
                    <> Text.pack (show code)
                    <> " and stderr: "
                    <> Text.pack stderr
          RunExitReturn -> pure exitCode
      output <-
        case outputBehavior of
          RunOutputShow -> void $ liftIO $ Text.putStr stdoutText
          RunOutputHide -> pure ()
          RunOutputReturn -> pure $ Text.strip stdoutText
      pure $ RunResult{..}

localDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
localDriver = runDriverIO $ IODriver{transformArgs = id, cwd = Nothing}

withAddedPath :: IOE :> es => Text -> Eff es a -> Eff es a
withAddedPath addPath action = do
  originalPath <- fmap Text.pack $ liftIO $ getEnv "PATH"
  let newPath = addPath <> ":" <> originalPath
  bracket_
    (liftIO $ setEnv "PATH" $ Text.unpack newPath)
    (liftIO $ setEnv "PATH" $ Text.unpack originalPath)
    action

testHostDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
testHostDriver act = do
  originalHome <- fmap Text.pack $ liftIO $ getEnv "HOME"
  bracket (localDriver $ drvTemp_ True) (localDriver . drvRm) $ \home -> do
    withAddedPath (home </> ".local" </> "bin") $ do
      let linkToOriginalHome path =
            let op = originalHome </> path
                np = home </> path
             in localDriver $ drvLink op np
      linkToOriginalHome ".local/share/fonts"
      linkToOriginalHome ".local/share/systemd/user"
      linkToOriginalHome "Library/LaunchAgents"
      let transformArgs ("sh" :| ["-c", "eval echo '~'"]) = "echo" :| [home]
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
      containerName <- Text.pack . show <$> liftIO (randomIO @Int)
      drvTempDir $ \tempDir -> do
        drvCopy "bootstrap" (tempDir </> "bootstrap")
        drvWriteFile (tempDir </> "Dockerfile") $ dockerfile baseImage
        let image = dockerImagePrefix <> baseImage
        drvRun $ "docker" :| ["build", "--tag", image, tempDir]
        -- FIXME: --volume for package root
        drvRunOutput $
          "docker"
            :| [ "run"
               , "--rm"
               , "--detach"
               , "--name"
               , "mybox-test-" <> containerName
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
           , homeOf user
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
    , "ENV PATH=" <> homeOf dockerUser <> "/.local/bin:$PATH"
    , "USER " <> dockerUser
    , -- populate dnf cache so each test doesn't have to do it
      "RUN command -v dnf >/dev/null && dnf check-update || true"
    ]

dockerUser :: Text
dockerUser = "regular_user"

homeOf :: Text -> Text
homeOf "root" = "/root"
homeOf user = "/home" </> user

dockerImagePrefix :: Text
dockerImagePrefix = "mybox-test-"

testDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
testDriver act = do
  image_ <- fmap (fromMaybe "") $ liftIO $ lookupEnv "DOCKER_IMAGE"
  case image_ of
    "" -> testHostDriver act
    image -> dockerDriver (Text.pack image) act
