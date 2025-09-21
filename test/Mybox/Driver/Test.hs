module Mybox.Driver.Test (
  pureDriver,
  stubDriver,
  testDriver,
) where

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful.Dispatch.Dynamic
import System.Environment

import Mybox.Driver.Class
import Mybox.Driver.IO
import Mybox.Driver.Ops
import Mybox.Platform
import Mybox.Prelude
import Mybox.Spec.Utils

pureDriver :: (Args -> Maybe Text) -> Eff (Driver : es) a -> Eff es a
pureDriver run = interpret_ $ \case
  DrvRun exitBehavior outputBehavior args ->
    pure $
      rrSuccess exitBehavior outputBehavior $
        requireJust ("pureDriver: unexpected command " <> show args) $
          run args

stubDriver :: Driver :> es => (Args -> Maybe Text) -> Eff es a -> Eff es a
stubDriver run = interpose_ $ \case
  DrvRun exitBehaviour outputBehaviour args -> case run args of
    Nothing -> send $ DrvRun exitBehaviour outputBehaviour args
    Just result -> pure $ rrSuccess exitBehaviour outputBehaviour result

rrSuccess :: RunExit e -> RunOutput o -> Text -> RunResult e o
rrSuccess exitBehaviour outputBehaviour result = RunResult{..}
 where
  exit = case exitBehaviour of
    RunExitError -> ()
    RunExitReturn -> ExitSuccess
  output = case outputBehaviour of
    RunOutputShow -> ()
    RunOutputHide -> ()
    RunOutputReturn -> Text.encodeUtf8 result

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
          localDriver act

containerDriver :: Driver :> es => Text -> Eff es a -> Eff es a
containerDriver container = modifyDriver transformArgs
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

testDockerDriver :: IOE :> es => Text -> Eff (Driver : es) a -> Eff es a
testDockerDriver baseImage act =
  bracket mkContainer rmContainer $ \container ->
    localDriver $ containerDriver container act
 where
  mkContainer :: IOE :> es => Eff es Text
  mkContainer =
    localDriver $ do
      containerName <- randomText "tests"
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

dockerfile :: Text -> Text
dockerfile baseImage =
  Text.unlines
    [ "FROM " <> baseImage
    , "RUN useradd --create-home --password '' " <> dockerUser
    , "RUN mkdir /mybox"
    , "WORKDIR /mybox"
    , "COPY bootstrap /bootstrap"
    , "RUN /bootstrap --development --haskell"
    , "ENV PATH=" <> (pRoot </> "home" </> dockerUser).text <> "/.local/bin:$PATH"
    , "USER " <> dockerUser
    , -- populate dnf cache so each test doesn't have to do it
      "RUN command -v dnf >/dev/null && dnf check-update || true"
    ]

dockerUser :: Text
dockerUser = "regular_user"

dockerImagePrefix :: Text
dockerImagePrefix = "mybox-test-"

testDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
testDriver act =
  localDriver (drvEnv "DOCKER_IMAGE") >>= \case
    Nothing -> testHostDriver act
    Just image -> testDockerDriver image act
