const isBuild = process.env.GITHUB_JOB === "build";

export default {
  branches: ["main"],
  plugins: [
    "@semantic-release/commit-analyzer",
    ...(isBuild
      ? [
          // Build job; only update the version in package.yaml
          [
            "semantic-release-replace-plugin",
            {
              replacements: [
                {
                  files: ["package.yaml"],
                  from: "^version: 0\\.0\\.0$",
                  to: "version: ${nextRelease.version}",
                },
              ],
            },
          ],
          "semantic-release-stop",
        ]
      : [
          // Actual release; the build jobs must have run before and produced
          // the executables
          "@semantic-release/release-notes-generator",
          [
            "@semantic-release/github",
            {
              assets: [{ path: "bootstrap" }, { path: "executables/*" }],
            },
          ],
        ]),
  ],
};
