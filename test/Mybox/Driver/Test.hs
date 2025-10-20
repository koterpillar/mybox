module Mybox.Driver.Test (
  pureDriver,
  stubDriver,
  testDriver,
) where

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful.Dispatch.Dynamic

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

testHostDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
testHostDriver act = localDriver $ do
  githubToken <- drvGithubToken
  originalHome <- drvHome
  originalPath <- fromMaybe "" <$> drvEnv "PATH"
  drvTempDir $ \home' -> do
    home <- drvRealPath home'
    let newLocalBin = home </> ".local" </> "bin"
    let envOverrides =
          [ ("GITHUB_TOKEN", githubToken)
          , ("PATH", newLocalBin.text <> ":" <> originalPath)
          , ("HOME", home.text)
          ]
    let linkToOriginalHome :: Driver :> es => Path Rel -> Eff es ()
        linkToOriginalHome path = do
          let op = originalHome <//> path
          let np = home <//> path
          drvMkdir op
          drvLink op np
    linkedDirectories <- flip fmap drvOS $ \case
      Linux _ -> [mkPath ".local/share/fonts", mkPath ".local/share/systemd/user"]
      MacOS -> [mkPath "Library/Fonts", mkPath "Library/LaunchAgents"]
    for_ linkedDirectories linkToOriginalHome
    modifyDriver (env envOverrides) act

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

testDockerDriver :: (Concurrent :> es, IOE :> es) => MVar () -> Text -> Eff (Driver : es) a -> Eff es a
testDockerDriver lock baseImage act =
  localDriver $
    bracket mkContainer rmContainer $ \container ->
      containerDriver container act
 where
  mkImage :: (Concurrent :> es, Driver :> es) => Eff es Text
  mkImage =
    atomicMVar lock $
      drvTempDir $ \tempDir -> do
        drvCopy (pSegment "bootstrap") (tempDir </> "bootstrap")
        drvWriteFile (tempDir </> "Dockerfile") $ dockerfile baseImage
        let image = dockerImagePrefix <> baseImage
        drvRun $ "docker" :| ["build", "--tag", image, tempDir.text]
        pure image
  mkContainer :: (Concurrent :> es, Driver :> es, IOE :> es) => Eff es Text
  mkContainer = do
    image <- mkImage
    containerName <- randomText "tests"
    githubToken <- drvGithubToken
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
  rmContainer :: Driver :> es => Text -> Eff es ()
  rmContainer container = drvRunSilent $ "docker" :| ["rm", "--force", container]

dockerfile :: Text -> Text
dockerfile baseImage =
  Text.unlines
    [ "FROM " <> baseImage
    , "RUN useradd --create-home --password '' " <> dockerUser
    , "RUN mkdir /mybox"
    , "WORKDIR /mybox"
    , "COPY bootstrap /bootstrap"
    , "RUN /bootstrap --development"
    , "ENV PATH=" <> (pRoot </> "home" </> dockerUser).text <> "/.local/bin:$PATH"
    , -- download often-used apt packages to speed up tests
      "RUN if command -v apt >/dev/null; then apt install -y --download-only nodejs npm python3-pip python3-venv unzip xz-utils; fi"
    , "USER " <> dockerUser
    , -- populate dnf cache so each test doesn't have to do it
      "RUN command -v dnf >/dev/null && dnf check-update || true"
    ]

dockerUser :: Text
dockerUser = "regular_user"

dockerImagePrefix :: Text
dockerImagePrefix = "mybox-test-"

testDriver :: (Concurrent :> es, IOE :> es) => MVar () -> Eff (Driver : es) a -> Eff es a
testDriver lock act =
  localDriver (drvEnv "DOCKER_IMAGE") >>= \case
    Nothing -> testHostDriver act
    Just image -> testDockerDriver lock image act
