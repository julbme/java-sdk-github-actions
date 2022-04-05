[![Build](https://github.com/julbme/java-sdk-github-actions/actions/workflows/maven-build.yml/badge.svg)](https://github.com/julbme/java-sdk-github-actions/actions/workflows/maven-build.yml)
[![Lint Commit Messages](https://github.com/julbme/java-sdk-github-actions/actions/workflows/commitlint.yml/badge.svg)](https://github.com/julbme/java-sdk-github-actions/actions/workflows/commitlint.yml)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=julbme_java-sdk-github-actions&metric=alert_status)](https://sonarcloud.io/summary/new_code?id=julbme_java-sdk-github-actions)
![Maven Central](https://img.shields.io/maven-central/v/me.julb/sdk-github-actions)

# Java SDK for GitHub actions

This SDK provides utility methods to build GitHub Actions in Java.

It is heavily inspired of GitHub Actions [core toolkit](https://github.com/actions/toolkit/tree/main/packages/core) available in *TypeScript*.

This SDK provides the following capabilities:

* Read inputs: `getInput`, `getMultilineInput`, `getBooleanInput`
* Write output variables: `setOutput`, `setEmptyOutput`
* Mask secret variables: `setSecret`
* Run operations within groups: `startGroup`, `endGroup`, `group` with closure
* Save and retrieve state: `saveState`, `getState`
* Log messages: `debug`, `notice`, `warning`, `error`
* Quick access to GitHub Actions [default environment variables](https://docs.github.com/en/actions/learn-github-actions/environment-variables#default-environment-variables)

## Documentation

* [Getting started](https://github.com/julbme/java-sdk-github-actions/wiki/Getting-Started)
* [Usage guide](https://github.com/julbme/java-sdk-github-actions/wiki/Usage)
* [JavaDoc](https://julbme.github.io/java-sdk-github-actions/apidocs/index.html)

## Contributing

This project is totally open source and contributors are welcome.
