# Neovim Tips

To check all the details in the neovim tutor:
1. $ nv
2. :Tutor
3. :checkhealth to make sure plugins are good and follow tips to fix errors
4. :Lazy to update plugins
5. ~/.config/nvim/sample/ contains every programming language sample you can try.
6. ~/.config/nvim/init.lua is the main entry of the nvim config.

## Tips for Marksman

Marksman plugin[reference]:

0. [reference] like above, "gd" or "gr" works.
1. [lesson 3 summary](#lesson-3-summary) can be used to go to any hash inside this md by typing "gd" on it, "gr" for references.
2. [note todo list](/note-taking/note.md#entertainment) can be used to go to any hash in another file by typing "gd" on it, "gr" for references.
3. [[note-taking]] can be used to go to any md file by typing "gd" on it, "gr" for references.
4. [[#Lesson 3 SUMMARY]] same to the first one.

## Tips for navigation
1. "gd" on tagged keywords means go to the definition.
2. "gr" on tagged keywords means go to the reference.
3. "gf" on file or url under the cursor will open the file and html source.
4. "gI" on tagged keywords means go to the implementation.
5. "gx" on file or url under the cursor will open the file or html with default app like Note app or chrome browser.

## Tips for Terminal, Split

0. `:Terminal` will open terminal.
1. `:Vspt` will open terminal inside neovim vertically.
2. `:Spt` will open terminal inside neovim horizontally.
3. `:vsp newfile` will open newfile inside neovim vertically.
4. `:sp newfile` will open newfile inside neovim horizontally.

## Tips for Tab, WD and Dashboard

1. `:tabnew newfile` to open a file in a new tab.
2. gt -> next tab, gT -> previous tab, 2gt -> next 2 tab
3. `<leader>.` to set file location as current working directory
4. `:Alpha` to go back to dashboard page.

## Tips for Git

1. `:Gc 'any commnets'` to cd to current file location and run !git commit -a -m 'any comments'
2. `:Gl` for git pull current folder
3. `:Gd` for git diff current folder
4. `:Gp` for git push current folder
5. `:Gst` for git status current folder

## Tips for nvim-cmp

1. Once snippet is activated, <C-l> & <C-h> can move cursor to next parameter

## Tips for diff

```bash
vi -d test1 test2
```

## Tips for AI Plugins

1. Run with `aider --no-auto-commits --vim --code-theme monokai` and then type `/help question` on how to use aider, type `/ask question` on chat mode
2. aider can be started by `viai` and `viail` in a neovim terminal(easy to share clipboard across neovim) in separate tmux window(easy to resize window), `viai` and `viail` are both defined inside ~/docker/.zshrc
3. ask aider to do one task at a time instead of doing too much with one prompt, using aider iteratively
4. before telling aider to do what you want, use `/ask` to discuss your requirement with aider first, review and then tell aider to make changes.
5. avante.nvim is for single file AI modification, customized shortcuts inside ~/.config/nvim/lua/avante/config.lua, start avante.nvim by SPACE + v in neovim

## Lesson 2.3: ON OPERATORS AND MOTIONS

Many commands that change text are made from an operator and a motion.
The format for a delete command with the [d](d) delete operator is as follows:

    d   motion

  Where:
    d      - is the delete operator.
    motion - is what the operator will operate on (listed below).

  A short list of motions:
    w - until the start of the next word, EXCLUDING its first character.
    e - to the end of the current word, INCLUDING the last character.
    $ - to the end of the line, INCLUDING the last character.

  Thus typing 'de' will delete from the cursor to the end of the word.

NOTE:  Pressing just the motion while in Normal mode without an operator
       will move the cursor as specified.

## Lesson 2.4: USING A COUNT FOR A MOTION

** Typing a number before a motion repeats it that many times. **

 1. Move the cursor to the start of the line marked ✓ below.

 2. Type 2w to move the cursor two words forward.

 3. Type 3e to move the cursor to the end of the third word forward.

 4. Type 0 (zero) to move to the start of the line.

 5. Repeat steps 2 and 3 with different numbers.

## Lesson 2.5: USING A COUNT TO DELETE MORE

** Typing a number with an operator repeats it that many times. **

In the combination of the delete operator and a motion mentioned above you
insert a count before the motion to delete more:
     d   number   motion

 1. Move the cursor to the first UPPER CASE word in the line marked ✗.

 2. Type d2w to delete the two UPPER CASE words

 3. Repeat steps 1 and 2 with a different count to delete the consecutive
    UPPER CASE words with one command

## Lesson 2.7: THE UNDO COMMAND

** Press u to undo the last commands, `U`{normal} to fix a whole line. **

 1. Move the cursor to the line below marked ✗ and place it on the first error.

 2. Type x to delete the first unwanted character.

 3. Now type u to undo the last command executed.

 4. This time fix all the errors on the line using the x command.

 5. Now type a capital U to return the line to its original state.

 6. Now type u a few times to undo the U and preceding commands.

 7. Now type C-r (Control + R) a few times to redo the commands.

Fiix the errors oon thhis line and reeplace them witth undo.

## Lesson 3 SUMMARY

 1. To put back text that has just been deleted, type [p](p). This puts the
    deleted text AFTER the cursor (if a line was deleted it will go on the
    line below the cursor).

 2. To replace the character under the cursor, type [r](r) and then the
    character you want to have there.

 3. The [change operator](c) allows you to change from the cursor to where
    the motion takes you. Type `ce`{normal} to change from the cursor to the
    end of the word, `c$`{normal} to change to the end of a line, etc.

 4. The format for change is:

        c   [number]   motion



## Lesson 5.3: SELECTING TEXT TO WRITE

** To save part of the file, type `v` motion `:w FILENAME`. **

 1. Move the cursor to this line.

 2. Press v and move the cursor to the fifth item below. Notice that the
    text is highlighted.

 3. Press the `:` character. At the bottom of the screen

        `:'<,'>`

    will appear.

 4. Type

        `:w TEST`

    where TEST is a filename that does not exist yet. Verify that you see

        `:'<,'>w TEST`

    before you press `<Enter>`.

 5. Neovim will write the selected lines to the file TEST. Use `:!ls` to see it.
    Do not remove it yet! We will use it in the next lesson.

NOTE: Pressing v starts [Visual selection](visual-mode). You can move the cursor around to
      make the selection bigger or smaller. Then you can use an operator to
      do something with the text. For example, `d` deletes the text.

## Lesson 5.4: RETRIEVING AND MERGING FILES

** To retrieve the contents of a file, type `:r FILENAME`. **

 1. Place the cursor just above this line.

NOTE:  After executing Step 2 you will see text from Lesson 5.3. Then move
       DOWN to see this lesson again.

 2. Now retrieve your TEST file using the command

        `:r TEST`

    where TEST is the name of the file you used.
    The file you retrieve is placed below the cursor line.

 3. To verify that a file was retrieved, cursor back and notice that there
    are now two copies of Lesson 5.3, the original and the retrieved version.

NOTE: You can also read the output of an external command. For example,

        `:r !ls`

      reads the output of the `ls` command and puts it below the cursor.


## Lesson 7.1: GETTING HELP

** Use the online help system. **

Neovim has a comprehensive online help system.

To get started, try one of these three:

  - press the `<F1>` key (if you have one)
  - type `:help`

Read the text in the help window to find out how the help works.
Type `<C-w><C-w>` to jump from one window to another.
Type `:q` to close the help window.


## Lesson 7.3: COMPLETION

** Command line completion with `<C-d>`{normal} and `<Tab>`{normal}. **

 1. List the contents of the current directory: `:!ls`{vim}

 2. Type the start of a command: `:e`{vim}

 3. Press `<C-d>`{normal} and Neovim will show a list of commands beginning with "e".

 4. Press `<Tab>`{normal} and Neovim will complete the command name to ":edit".

 5. Now add a space and the start of an existing file name: `:edit FIL`{vim}

 6. Press `<Tab>`{normal}. Neovim will complete the name ("FIL" -> "FILE", if it is unique).

[reference]: https://github.com/artempyanykh/marksman "marksman"
