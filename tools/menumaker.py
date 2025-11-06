import tkinter as tk
from tkinter import ttk
import dcfreader
import parser3 as parser

symfile = '''
#include "src/inc/macros.inc"
#include "src/inc/osdefs.inc"
#include "src/inc/gamedefs.inc"
'''

class MenuMakerApp:
    PIXEL_COLS = 96
    PIXEL_ROWS = 64
    PIXEL_ON_COLOR = "black"
    PIXEL_OFF_COLOR = "white"

    def __init__(self, master):
        tokenstream = parser.TokenStream.from_str(symfile)
        self.initialsymtable = parser.Parser(tokenstream).symtable
        self.master = master
        master.title("Application Title")
        master.state('zoomed')

        # Configure grid to be resizable
        master.grid_rowconfigure(0, weight=1)
        master.grid_columnconfigure(0, weight=1)
        master.grid_columnconfigure(1, weight=1)

        # --- Left Panel: Pixel Grid ---
        self.pixel_grid_frame = ttk.Frame(master, relief="sunken", borderwidth=2)
        self.pixel_grid_frame.grid(row=0, column=0, sticky="nsew", padx=5, pady=5)
        self.pixel_grid_frame.grid_rowconfigure(0, weight=1)
        self.pixel_grid_frame.grid_columnconfigure(0, weight=1)
        self.pixel_grid_frame.grid_propagate(False) # Prevent frame from shrinking/growing based on canvas

        self.pixel_grid_canvas = tk.Canvas(self.pixel_grid_frame, bg="lightgray", highlightbackground="black", highlightthickness=1)
        # Use place geometry manager for precise size control
        self.pixel_grid_canvas.place(relx=0.5, rely=0.5, anchor="center") # Center the canvas initially

        self.pixels = [] # Stores references to the pixel rectangles
        # Initial call with default size, will be redrawn on configure
        self._create_pixel_grid(self.PIXEL_COLS * 5, self.PIXEL_ROWS * 5)
        # Bind the resize event to the frame, not the canvas itself
        self.pixel_grid_frame.bind("<Configure>", self._on_canvas_resize)
        self._draw_border_pattern() # Call the border test pattern
        self._draw_char_test() # Call the new character drawing test
        self.pixel_grid_canvas.update_idletasks() # Force canvas update

        # Set a minimum size for the main window to ensure pixels are visible (1x1 pixel minimum)
        master.minsize(self.PIXEL_COLS * 1, self.PIXEL_ROWS * 1)
        self._last_pixel_size = 0 # To track changes in pixel size for delayed redraw
        self._resize_job = None # To store the ID of the scheduled redraw job

        # --- Right Panel ---
        self.right_panel_frame = ttk.Frame(master)
        self.right_panel_frame.grid(row=0, column=1, sticky="nsew", padx=5, pady=5)
        self.right_panel_frame.grid_rowconfigure(1, weight=1) # Text input area should expand
        self.right_panel_frame.grid_columnconfigure(0, weight=1)

        # Text Parser Status
        self.parser_status_label = ttk.Label(self.right_panel_frame, text="text parser status goes here", anchor="w")
        self.parser_status_label.grid(row=0, column=0, sticky="ew", padx=5, pady=2)

        # Text Input Area
        self.text_input_frame = ttk.Frame(self.right_panel_frame, relief="sunken", borderwidth=1)
        self.text_input_frame.grid(row=1, column=0, sticky="nsew", padx=5, pady=5)
        self.text_input_frame.grid_rowconfigure(0, weight=1)
        self.text_input_frame.grid_columnconfigure(0, weight=1)

        self.text_input = tk.Text(self.text_input_frame, wrap="word", height=10)
        self.text_input.grid(row=0, column=0, sticky="nsew")

        self.text_input_scrollbar = ttk.Scrollbar(self.text_input_frame, orient="vertical", command=self.text_input.yview)
        self.text_input_scrollbar.grid(row=0, column=1, sticky="ns")
        self.text_input.config(yscrollcommand=self.text_input_scrollbar.set)

        # Bind Enter key to parse_input method
        self.text_input.bind("<F5>", self.parse_input)

    def print_char_to_screen(self, char_code, start_x, start_y):
        """
        Prints a specified character from the DCF font to the virtual screen
        at the given starting coordinates.
        """
        try:
            # Ensure dcfreader is initialized
            if not hasattr(self, '_dcf_reader'):
                self._dcf_reader = dcfreader.DCFReader("tools/escheron.dcf")

            char_data = self._dcf_reader.get_char(char_code)
            
            if char_data:
                for row_idx, row_data in enumerate(char_data.disparr):
                    for col_idx, pixel_color in enumerate(row_data):
                        target_x = start_x + col_idx
                        target_y = start_y + row_idx
                        # Adjust color mapping based on dcfreader's inverted logic
                        if pixel_color == dcfreader.Colors.BLACK: # dcfreader.Colors.BLACK means the pixel should be WHITE on screen
                            self.pixelOff(target_x, target_y)
                        else: # dcfreader.Colors.WHITE means the pixel should be BLACK on screen
                            self.pixelOn(target_x, target_y)
            else:
                print(f"Character code {char_code} not found in DCF file.")
        except Exception as e:
            print(f"Error printing character {char_code}: {e}")

    def _draw_char_test(self):
        print("--- Drawing Character 'A' Test ---")
        self.print_char_to_screen(ord('A'), 40, 40)
        print("--- Character 'A' Test Complete ---")

    def _draw_border_pattern(self):
        print("--- Drawing Border Pattern ---")
        border_thickness = 2
        for row in range(self.PIXEL_ROWS):
            for col in range(self.PIXEL_COLS):
                # Check if pixel is within the border region
                if (row < border_thickness or row >= self.PIXEL_ROWS - border_thickness or
                    col < border_thickness or col >= self.PIXEL_COLS - border_thickness):
                    if (row + col) % 2 == 0:
                        self.pixelOn(col, row)
                    else:
                        self.pixelOff(col, row)
                else:
                    # Ensure inner pixels are black (default 'on' state)
                    self.pixelOn(col, row)
        print("--- Border Pattern Complete ---")

    def _draw_checkerboard_pattern(self):
        print("--- Drawing Checkerboard Pattern ---")
        for row in range(self.PIXEL_ROWS):
            for col in range(self.PIXEL_COLS):
                if (row + col) % 2 == 0:
                    self.pixelOn(col, row)
                else:
                    self.pixelOff(col, row)
        print("--- Checkerboard Pattern Complete ---")

    def _draw_test_pattern(self):
        print("--- Running Pixel Grid Tests ---")

        # Test pixelOn
        self.pixelOn(0, 0) # Top-left
        self.pixelOn(self.PIXEL_COLS - 1, 0) # Top-right
        self.pixelOn(0, self.PIXEL_ROWS - 1) # Bottom-left
        self.pixelOn(self.PIXEL_COLS - 1, self.PIXEL_ROWS - 1) # Bottom-right
        self.pixelOn(self.PIXEL_COLS // 2, self.PIXEL_ROWS // 2) # Center

        # Test pixelOff
        self.pixelOff(1, 0)
        self.pixelOff(self.PIXEL_COLS - 2, 0)
        self.pixelOff(1, self.PIXEL_ROWS - 1)
        self.pixelOff(self.PIXEL_COLS - 2, self.PIXEL_ROWS - 1)

        # Test pixelChange (toggle)
        self.pixelChange(2, 0) # Should turn white (from initial black)
        self.pixelChange(2, 0) # Should turn black again

        # Test getPixel
        print(f"Pixel (0,0) state: {self.getPixel(0, 0)} (Expected: 1)")
        print(f"Pixel (1,0) state: {self.getPixel(1, 0)} (Expected: 0)")
        print(f"Pixel (2,0) state: {self.getPixel(2, 0)} (Expected: 1)")
        print(f"Pixel ({self.PIXEL_COLS // 2},{self.PIXEL_ROWS // 2}) state: {self.getPixel(self.PIXEL_COLS // 2, self.PIXEL_ROWS // 2)} (Expected: 1)")

        # Test out of bounds exception
        try:
            self.getPixel(-1, 0)
        except IndexError as e:
            print(f"Caught expected error for (-1,0): {e}")
        try:
            self.pixelOn(self.PIXEL_COLS, 0)
        except IndexError as e:
            print(f"Caught expected error for ({self.PIXEL_COLS},0): {e}")

        print("--- Pixel Grid Tests Complete ---")

    def _create_pixel_grid(self, canvas_width, canvas_height):
        self.pixel_grid_canvas.delete("all") # Clear any existing drawings
        self.pixels = []

        # Calculate pixel dimensions based on integer pixel size
        pixel_width = canvas_width / self.PIXEL_COLS
        pixel_height = canvas_height / self.PIXEL_ROWS

        for row in range(self.PIXEL_ROWS):
            for col in range(self.PIXEL_COLS):
                x1 = col * pixel_width
                y1 = row * pixel_height
                x2 = x1 + pixel_width
                y2 = y1 + pixel_height
                pixel_id = self.pixel_grid_canvas.create_rectangle(x1, y1, x2, y2,
                                                                   fill=self.PIXEL_ON_COLOR,
                                                                   outline="") # Borderless
                self.pixels.append(pixel_id)

    def _perform_redraw(self, new_canvas_width, new_canvas_height, current_pixel_size):
        # Only redraw if the pixel size has actually changed to a new integer value
        if current_pixel_size != self._last_pixel_size:
            self._last_pixel_size = current_pixel_size

            # Configure canvas size
            self.pixel_grid_canvas.place_configure(width=new_canvas_width, height=new_canvas_height)
            self._create_pixel_grid(new_canvas_width, new_canvas_height)
            self._draw_border_pattern()
            self._draw_char_test() # Re-draw the character after border pattern
            self.pixel_grid_canvas.update_idletasks()

            # Adjust the width of both frames to match the canvas width
            # We need to account for the padx/pady of the frames
            frame_padx = 5 # As defined in grid call for both frames

            # Set the width of the pixel_grid_frame
            self.pixel_grid_frame.config(width=new_canvas_width + (2 * frame_padx))

            # Set the width of the right_panel_frame
            self.right_panel_frame.grid_propagate(False) # Prevent frame from shrinking to fit contents
            self.right_panel_frame.config(width=new_canvas_width + (2 * frame_padx))

            # The text_input_frame is inside right_panel_frame, so its width should be new_canvas_width
            self.text_input_frame.config(width=new_canvas_width)

    def _on_canvas_resize(self, event):
        # Get current frame dimensions
        frame_width = event.width
        frame_height = event.height

        # Calculate maximum possible integer pixel size
        max_pixel_size_by_width = frame_width // self.PIXEL_COLS
        max_pixel_size_by_height = frame_height // self.PIXEL_ROWS

        # Choose the smaller of the two to maintain aspect ratio, and ensure minimum 1x1 pixel
        current_pixel_size = max(1, min(max_pixel_size_by_width, max_pixel_size_by_height))

        # Calculate new canvas dimensions based on integer pixel size
        new_canvas_width = current_pixel_size * self.PIXEL_COLS
        new_canvas_height = current_pixel_size * self.PIXEL_ROWS

        # Schedule redraw if pixel size changed or if it's the initial draw
        if current_pixel_size != self._last_pixel_size or self._last_pixel_size == 0:
            if self._resize_job:
                self.master.after_cancel(self._resize_job)
            self._resize_job = self.master.after(100, self._perform_redraw, new_canvas_width, new_canvas_height, current_pixel_size)

    def _get_pixel_index(self, x, y):
        if not (0 <= x < self.PIXEL_COLS and 0 <= y < self.PIXEL_ROWS):
            raise IndexError(f"Coordinates ({x}, {y}) are out of bounds. Must be within (0-{self.PIXEL_COLS-1}, 0-{self.PIXEL_ROWS-1}).")
        return y * self.PIXEL_COLS + x

    def pixelOn(self, x, y):
        index = self._get_pixel_index(x, y)
        self.pixel_grid_canvas.itemconfig(self.pixels[index], fill=self.PIXEL_ON_COLOR)

    def pixelOff(self, x, y):
        index = self._get_pixel_index(x, y)
        self.pixel_grid_canvas.itemconfig(self.pixels[index], fill=self.PIXEL_OFF_COLOR)

    def pixelChange(self, x, y):
        index = self._get_pixel_index(x, y)
        current_color = self.pixel_grid_canvas.itemcget(self.pixels[index], "fill")
        if current_color == self.PIXEL_ON_COLOR:
            self.pixelOff(x, y)
        else:
            self.pixelOn(x, y)

    def getPixel(self, x, y):
        index = self._get_pixel_index(x, y)
        current_color = self.pixel_grid_canvas.itemcget(self.pixels[index], "fill")
        return 1 if current_color == self.PIXEL_ON_COLOR else 0

    def parse_input(self, event=None):
        input_text = self.text_input.get("1.0", tk.END).strip()
        #print(f"Input received: {input_text}") # For debugging
        tokenstream = parser.TokenStream.from_str(input_text)
        try:
            #TODO: Implement label and the two-pass system for forward-reference
            parseobj = parser.Parser(tokenstream, self.initialsymtable)
            datastream = parseobj.binres
        except Exception as e:
            datastream = e

        #
        # TODO: ACTUALLY PROCESS THE DATASTREAM FOR CONTENTS AND ACT ON THEM
        #


        # Placeholder for parsing logic and updating pixel grid/status
        self.parser_status_label.config(text=f"Emitted: '{datastream.hex()}'")
        return "break" # Prevents the default newline character from being inserted

if __name__ == "__main__":
    root = tk.Tk()
    app = MenuMakerApp(root)
    root.mainloop()
