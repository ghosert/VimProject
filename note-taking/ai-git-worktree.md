# Git Worktree: Parallel Development Guide

`git worktree` allows you to have multiple working directories attached to a single Git repository. This enables you to work on multiple branches simultaneously without stashing or switching contexts in a single folder.

## Key Benefits
- **Simultaneous Work:** Run tests on one branch while coding on another in a separate folder.
- **Shared History:** All worktrees share the same `.git` database. Commits made in one are immediately visible in others (locally).
- **Efficiency:** Saves disk space by sharing the object database and is nearly instantaneous compared to `git clone`.
- **Isolation:** Perfect for keeping different AI sessions focused on specific tasks without file interference.

## Core Commands

### Adding a Worktree
There are three ways to add a worktree and its branch:

1.  **New Branch:** `git worktree add ../path -b branch-name [start-point]`
    *   Creates a *new* branch and directory.
    *   **Pro-Tip:** Use `main` or `origin/main` as the `start-point` to "cut" a new branch from there.
2.  **Existing Branch:** `git worktree add ../path branch-name`
    *   Uses an *existing* branch (must not be in use elsewhere).
3.  **Auto-Named Branch:** `git worktree add ../path`
    *   Automatically creates a new branch named after the directory (`path`).

### Management
- **List:** `git worktree list`
- **Remove:** `git worktree remove ../path`
- **Cleanup:** `git worktree prune`

## The Golden Rule
**One branch per worktree.** Git prevents you from checking out the same branch in two different worktrees to avoid data corruption. Each parallel task must have its own branch.

---

## Best Practices: Using Worktrees with Multiple AIs

To maximize productivity when working with AI agents (like Gemini CLI), follow this parallel workflow:

### 1. Parallel Task Setup
When you have a second task while the first AI is still thinking/writing:
1.  **Create a new worktree:**
    ```bash
    git worktree add ../task-two -b feature/task-two
    ```
2.  **Open a new terminal tab:** `cd ../task-two`
3.  **Start a new AI session:** Initialize your AI tool in this new directory.
4.  **Repeat:** You can have 3, 4, or more AIs working on different branches in different folders simultaneously.

### 2. Handling Conflicts
While worktrees prevent *file-level* overwriting during development, you will still face *logical conflicts* when merging branches later.
- **Side-by-Side Resolution:** Open both worktrees on your screen to manually compare how different AIs solved specific problems.
- **Local Merging:** Since the history is shared, you can `git merge feature/task-one` while inside the `task-two` worktree without needing to push/pull from a server.

### 3. Session Isolation
- **Context Clarity:** By using separate worktrees, the AI in "Folder A" will not be confused by unfinished changes or experimental code in "Folder B."
- **Dependency Management:** If a task requires different environment variables or temporary build files, they remain isolated within that worktree's folder.

### 4. Comparison: Worktree vs. Clone
| Feature | Git Worktree | Multiple Git Clones |
| :--- | :--- | :--- |
| **Disk Space** | Minimal (Shared `.git`) | High (Redundant `.git` folders) |
| **Syncing** | Instant (Local) | Slow (Requires Push/Pull) |
| **Setup Speed** | Instant | Slow (Network dependent) |
| **Config/Hooks** | Shared across all | Must be set up per clone |

---

## Summary Workflow for AI Power Users
1.  **Main Repo:** Work on Task A.
2.  **New Task B?** `git worktree add -b feat-b ../task-b main`
3.  **New Task C?** `git worktree add -b feat-c ../task-c main`
4.  **Manage:** Use `git worktree list` to keep track of your "AI Army."
5.  **Cleanup:** Once a task is merged and deleted, `git worktree remove ../path` to keep your workspace clean.
6.  **Cleanup:** `git worktree prune`
