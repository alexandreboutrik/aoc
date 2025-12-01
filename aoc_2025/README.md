# Advent of Code 2025

![Language](https://img.shields.io/badge/language-weaponized_pascal-blue) ![Correctness](https://img.shields.io/badge/correctness-proven_ish-yellowgreen) ![Loop Invariants](https://img.shields.io/badge/loop_invariants-hallucinated-blueviolet)

For this year's challenge, I have chosen SPARK 2014 / Ada. My goal is to leverage the features of SPARK to write formally verifiable solutions where possible.

## Project Structure

```
aoc_2025/
├── alire.toml       # Project configuration
├── input/           # Puzzle input files (e.g., day\_01.txt)
├── src/
│   ├── main.adb     # Main entry point and argument parsing
│   └── days/        # Individual solution packages
│       ├── day_01.adb
│       └── ...
└── aoc_2025.gpr     # GNAT project file
```

## Building and Running

```
alr run -a <day_number>
# To prove the correctness of the code using the gnatprove:
alr exec -- gnatprove -P aoc_2025.gpr
```

## LICENSE

This project is licensed under the [MIT License](https://opensource.org/licenses/MIT). Feel free to use, modify, and distribute the code as needed. See the [LICENSE](LICENSE) file for more information.
