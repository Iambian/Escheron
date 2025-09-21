# Project Brief
## Title
Escheron: Twilight over Ragnoth

## Purpose
To rebuild an incomplete game designed to run on the TI-83 Plus graphing calculator and then to work on the resulting code until the game reaches completion.

## Game Type
Escheron: Twilight over Ragnoth is a mostly traditional role-playing game (RPG).

## Scope and Key Deliverables
The project aims to bring the game to completion through the following main phases:
1.  **Tooling Development:** Develop project tooling to facilitate a more efficient and organized build process.
2.  **Source Transfer and Refactoring:** Transfer and convert/refactor the existing game source code to be compatible with the new tooling and to enable clean project builds without writing build artifacts back into the source directory.
3.  **Component Testing and Verification:** Thoroughly test and verify that all converted game components are fully functional.
4.  **Game Completion:** Implement the remainder of the game, focusing on cutscenes and scripted logic necessary to advance the game's story and gameplay via automated actions. The project's prior form was mechanically feature-complete (or mostly so) but lacked these narrative and interactive elements.

## Success Metrics
The game is considered complete when:
*   It exhibits all features described in `docs/inner/design_doc.txt`.
*   The story contains all elements described in `docs/inner/game_flow.txt`.
*   All implied features common to this type of program (e.g., introduction, title screens) are implemented.

## Technical Specifications
*   Game sources use a combination of z80 assembly code, standard image formats, and proprietary data formats.
*   Tooling is a combination of Python 3.10 scripts, a data compression executable, and an assembler executable with linker capabilities.
*   Project documentation will primarily use Markdown, though existing `.txt` files are present.
