# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
 and this project adheres to [Semantic Versioning](https://semver.org).

## [Unreleased]

### Added

- Command-line interface setup with initial functionality.
- Subcommand: `publish`
  - Publishes SSCG and SBOM `.json` files to a specified HTTP endpoint.
- Subcommand: `generate`
  - Generates an SSCG (Software Supply Chain Graph) file using the SBOM and test folder as inputs.
    - Optional inclusion of authors metadata and a specified output path.
  - Evidences are created directly from the test file content without analyzing file types or contents in detail—a minimal.
