# CSC424 Template

This is a template project for assignments in the DePauw CSC424, Programming Languages course.
It is based on a template project designed for people using the Doodle library from [Creative Scala][creative-scala].

1. Fork or clone this template. What's the difference?
   - If you fork this repository you get your own copy on Github. You can then clone that repository to get a copy on your computer and save work back to your fork on Github and share it with other programmers. 
   - If you clone this repository without forking you get a copy on your computer but no copy on Github that you can save work back to and share.

2. Open your Terminal or Command Prompt in this directory.
   1. In VS Code, go to Terminal -> New Terminal. There should be a terminal window that pops up under the code editor.
   2. Or, in Mac, 
      1. Press Cmd + Space to open the Spotlight Search. 
      2. Type "Terminal" to find the Terminal.
      3. Click that to open it.
      4. Go to this directory: `cd <path to>/csc424-template`. Replace `<path to>` with your directory path.
   3. In Windows, 
      1. Go to the Start Menu.
      2. Search for Command Prompt.
      3. Click that to open it. 
      4. Go to this directory: `cd <path to>/csc424-template` Replace `<path to>` with your directory path.

3. If you do not already have Java and Node installed, see [https://adoptium.net/temurin/releases/] and [https://nodejs.org/en/download/package-manager/current].

3. Run `sbt` (if you already have SBT installed), `./sbt.sh` (OS X and Linux) or `sbt.bat` (Windows) to start SBT.
4. Type `run` in SBT to run the @main method, or `console` if you want to explore in an interactive console, or `test` to run Scala unit tests.

5. To run JavaScript unit tests, type `npm test` at the terminal prompt (_not_ the SBT prompt; use `exit` to get out of SBT if it is running).
6. To run a JavaScript file with a main method, run `node <filename>`. For example, `node src/main/js/assignment1/Group.js`.

7. You can also run this repository in a GitHub Codespace.

[creative-scala]: https://creativescala.org/
