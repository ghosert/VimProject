# AI Features

This document summarizes the AI features found in the [Neovim](~/.config/nvim/) [zsh](~/.zshrc) configuration.

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
    *   `vigem`: Opens Neovim and starts `gemini`.
    *   `vicla`: Opens Neovim and starts `claude`.
    *   `viopen`: Opens Neovim and starts `opencode`.
*   **Environment Variables:**
    *   `AVANTE_OPENAI_API_KEY`: API key for OpenAI.
    *   `AVANTE_ANTHROPIC_API_KEY`: API key for Anthropic (Claude).


## summary

1. claude or vicla (claude code, max plan subscription)
2. gemini or vigem (Gemini 3 pro, personal google account)
3. opencode or viopen for claude max plan, gemeni and local ollama gpt-oss:latest (qwen3-coder:30b is not working well with opencode right now)
4. avante.nvim (claude API usage[COSTING MONEY, how to replace it with max plan], gemini?, ollama qwen3-coder:30b)
5. claudecode.nvim for claude code.
6. opencode.nvim for open code.
7. Ollama is running on macbook pro M3 PRO with 36GB memory with qwen3-coder:30b

Best practice:

1. Use claude code for vibe coding, no neovim, paid
2. Use gemini for everyting else with paid Gemini AI Pro plan
3. Use opencode.nvim(claude, gemini), claudecode.nvim for pair programming in neovim
4. Use avante.nvim when offline for ollama qwen3-coder:30b, since avante does not support claude max plan rightnow, claude api key costs)
5. Use opencode when offline for ollama gpt-oss:latest (test if this is really working and update ./ai-features.png if it's not the truth)
6. Use ollama run model-name for any other local models, run it in neovim terminal

![ai-features](./ai-features.png)
