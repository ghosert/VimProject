# AI Features

This document summarizes the AI features found in the [Neovim](~/.config/nvim/) [zsh](~/.zshrc) configuration.

## ChatGPT.nvim

A plugin for integrating ChatGPT into Neovim.

*   Provides commands like `ChatGPT`, `ChatGPTEditWithInstruction`, `ChatGPTRun`, etc.
*   Has keybindings for various actions:
    *   `add_tests`: Adds tests to code.
    *   `docstring`: Adds docstrings to functions.
    *   `fix_bugs`: Fixes bugs in code.
    *   `explain_code`: Explains code in markdown format.
    *   `complete_code`: Completes code in various languages.
    *   `grammar_correction`: Corrects grammar to standard English.
    *   `translate`: Translates text to other languages.
    *   `keywords`: Extracts keywords from text.
    *   `optimize_code`: Optimizes code.
    *   `summarize`: Summarizes text.
    *   `roxygen_edit`: Inserts a roxygen skeleton to document R functions.
    *   `code_readability_analysis`: Analyzes code for readability issues.
*   Uses `gpt-4o` model.

## avante.nvim

An AI plugin with the following features:

*   **Providers:** Supports multiple AI providers like `claude`, `openai`, `azure`, `gemini`, `vertex`, `cohere`, `copilot`, `bedrock`, `ollama`.
*   **Default Provider:** The default provider is set to `claude`.
*   **Local LLM:** Can be configured to use a local LLM with `ollama`.
*   **Web Search:** Has a web search feature that can use `tavily`, `serpapi`, `searchapi`, `google`, `kagi`, `brave`, `searxng`.
*   **Shortcuts:** Defines a lot of shortcuts for common tasks like:
    *   Grammar Correction
    *   Keywords extraction
    *   Code Readability Analysis
    *   Optimize Code
    *   Summarize text
    *   Translate text
    *   Explain Code
    *   Complete Code
    *   Add Docstring
    *   Fix Bugs
    *   Add Tests

## zshrc

The `.zshrc` file contains aliases and environment variables for interacting with AI tools.

*   **Aliases:**
    *   `viai`: Opens Neovim and starts `aider`, an AI pair programming tool.
    *   `viail`: Opens Neovim and starts `aider` with a local `ollama` model.
    *   `vigem`: Opens Neovim and starts `gemini`.
*   **Environment Variables:**
    *   `OPENAI_API_KEY`: API key for OpenAI.
    *   `ANTHROPIC_API_KEY`: API key for Anthropic (Claude).
    *   `OLLAMA_API_BASE`: Specifies the base URL for a local `ollama` instance.
