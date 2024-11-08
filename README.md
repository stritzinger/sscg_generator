# Static Software Supply Chain Guarantee (SSCG) Generator

**Table of content**

- [Static Software Supply Chain Guarantee (SSCG) Generator](#static-software-supply-chain-guarantee-sscg-generator)
  - [Description](#description)
  - [Requeriments](#requeriments)
  - [Build](#build)
  - [Run](#run)

## Description

```mermaid
flowchart LR
    Z[[Source Code]] --> Y{Generate SBOM}
    Y --> C
    Z[[Source Code]] --> A
    A{Execute tests} --> B[[ Test result, .txt file]] 
    C[[SBOM .json]] -->|input| D
    B -->|Input| D{SSCG generator}
    D --> E[[SSG file .json]]
    C -->|Input| F
    E -->|Input| F{Send to another web service}
```

In the context of **RESCALE**, Stritzinger is overseeing the development of component known as the **Static Code Analysis Module**. This module analyzes the source code and produces two output files:

1. **Test Report**: A file containing the results of various static tests performed on the source code (in `.txt` format).
2. **SBOM (Software Bill of Materials)**: A structured document (in CycloneDX `.json` format) that lists all project dependencies.

These two outputs, the **test report** and the **SBOM**, serve as inputs to generate a document called the **SSCG** (Static Software Supply Chain Guarantee). The SSCG is a CycloneDX document that combines the metadata from the SBOM with additional information about the testing process, ensuring the integrity and security of the software supply chain. It provides a detailed overview of the project’s components, test results, and other critical metadata, such as the tools used in the analysis.

Once the SSCG is generated, it is submitted—along with the SBOM (that includes more information about the project and its dependencies)—to a web service for further processing. This is done by sending both files to an HTTP endpoint, where the SSCG undergoes additional validation or integration into a broader system. The process ensures that the project’s dependencies and test outcomes are well-documented and traceable, providing greater transparency and confidence in the software’s supply chain.

For reference, you can find an SSCG example here: [SSCG Example](./priv/result/sscg-example.json), and more information about SSCG here: [SSCG](/docs/sscg.md).

## Requeriments

For RESCALE, the following are required:

- Erlang 27.0
- Rebar 3.24.0

However, due to dependencies (specifically argparse), the minimum OTP version needed is 26.

## Build

Run this command to generate an escript executable containing the project’s and
its dependencies’ BEAM files.

```sh
rebar3 escriptize
```

## Run

To run sscg command-line app, execute the following command:

```sh
_build/default/bin/sscg_generator
```

💡 **Tip**

For convenience, you can create a symlink to easily execute `sscg_generator`
 from any location in your `PATH`, following these steps:

1. Ensure that `~/.local/bin` is included in your `PATH`. You can add it to your
 shell configuration file (e.g., `.bashrc`, `.bash_profile`, or `.zshrc`), if
 it’s not already present:

    ```sh
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
    ```

2. Navigate to the `~/.local/bin` directory and create a symbolic link to the `sscg_generator` executable:

    ```sh
    cd ~/.local/bin
    ln -s /path/to/sscg_generator/_build/default/bin/sscg_generator
    ```

    ⚠️ Replace `/path/to/sscg_generator` with the actual path to your sscg_generator project directory.

Then, you can run `sscg_generator` from anywhere in your terminal.
