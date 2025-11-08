import tkinter as tk
from tkinter import ttk
import dcfreader
import parser3 as parser

# TODO: FIX SIGNED VS NONSIGNED ARITHMETIC IN SCRIPT SYSTEM. THIS BUG IS VISIBLE
# WHEN TRYING TO PERFORM m_addxy() WITH NEGATIVE NUMBERS.


class GameMap(object):
    ''' This is a project-specific blurb. To be honest, the parse_input and
        anything else touching the character renderer is also project-specific,
        but that's a little easier to deal with than the real game-specific
        abstractions that are happening in here. The only thing that can't be
        abstracted away is the memory accesses, which should be done in
        self.memory. App pagination is not supported; the entire 64KB memory
        space is assumed to be (initialized-to-zero) RAM.
    '''
    symfile = '''
    #include "src/inc/macros.inc"
    #include "src/inc/osdefs.inc"
    #include "src/inc/gamedefs.inc"
    '''
    def __init__(self):
        cls = self.__class__
        tokenstream = parser.TokenStream.from_str(cls.symfile)
        self.incparse = parser.Parser(tokenstream)
        self.symtable = self.incparse.symtable
        self.memory = [0] * 65536

    def get(self, sym):
        return self.memory[self.symtable[sym]]
    def set(self, sym, val):
        self.memory[self.symtable[sym]] = val
    def get2(self, sym):
        adr = self.symtable[sym]
        return self.memory[adr] + (256* self.memory[adr+1])
    def set2(self, sym, val):
        adr = self.symtable[sym]
        self.memory[adr] = val & 0xFF
        self.memory[adr+1] = (val >> 8) & 0xFF
        
    def x(self, set=None):
        if set is not None:
            self.set("textx", set)
        return self.get("textx")

    def y(self, set=None):
        if set is not None:
            self.set("texty", set)
        return self.get("texty")
    
    def cr(self, set=None):
        if set is not None:
            self.set("textxstart", set)
        return self.get("textxstart")
    
    def acc1(self, set=None):
        if set is not None:
            self.set("textacc", set)
        return self.get("textacc")
    
    def acc(self, set=None):
        if set is not None:
            self.set2("textacc", set)
        return self.get2("textacc")
    
    def idx(self, set=None):
        if set is not None:
            self.set2("textidx", set)
        return self.get2("textidx")

    def accshad(self, set=None):
        if set is not None:
            self.set2("textaccshadow", set)
        return self.get2("textaccshadow")
    
    def NLH(self):
        return self.symtable["NEWLINE_HEIGHT"]
    
    def BMT(self):
        return self.symtable["BOX_MARGIN_LEFT"]
    
    def BML(self):
        return self.symtable["BOX_MARGIN_LEFT"]

        

    




class MenuMakerApp:
    PIXEL_COLS = 96
    PIXEL_ROWS = 64
    PIXEL_ON_COLOR = "black"
    PIXEL_OFF_COLOR = "white"

    def __init__(self, master):
        self.gamemap = GameMap()
        self.master = master
        self.dcf_reader = dcfreader.DCFReader("tools/escheron.dcf")
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
        self.pixel_grid_welcome_message()
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

    def pixel_grid_welcome_message(self):
        # Prints a welcome message on the freshly-initialized pixel grid
        self.print_str("Display field initialized", 10, 23)
        self.print_str("Please input menu code", 10, 31)
        self.print_str("for simulation.", 20, 39)
        pass
        

    def print_str(self, string, x, y):
        for c in string:
            if c == '\n':
                y += 8
                continue
            cdat = self.dcf_reader.get_char(ord(c))
            self.render_char(cdat, x, y)
            x += cdat.width

    def print_char(self, char_code, x, y):
        """
        Prints a specified character from the DCF font to the virtual screen
        at the given starting coordinates.
        """
        char_data = self.dcf_reader.get_char(char_code)
        self.render_char(char_data, x, y)
        x += char_data.width
        return (x,y)

    def render_char(self, char_data:dcfreader.DCFchar, x, y):
        for rowidx, rowdat in enumerate(char_data.disparr):
            for colidx, pixel in enumerate(rowdat):
                tx = x + colidx
                ty = y + rowidx
                try:
                    if pixel == dcfreader.Colors.BLACK:
                        self.pixelOff(tx, ty)
                    else:
                        self.pixelOn(tx, ty)
                except:
                    pass

    def _create_pixel_grid(self, canvas_width, canvas_height):
        # Calculate pixel dimensions based on integer pixel size
        pixel_width = canvas_width / self.PIXEL_COLS
        pixel_height = canvas_height / self.PIXEL_ROWS

        if not self.pixels: # If pixels list is empty, create them for the first time
            self.pixel_grid_canvas.delete("all") # Clear any existing drawings
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
        else: # Otherwise, update the coordinates of existing pixels
            for row in range(self.PIXEL_ROWS):
                for col in range(self.PIXEL_COLS):
                    x1 = col * pixel_width
                    y1 = row * pixel_height
                    x2 = x1 + pixel_width
                    y2 = y1 + pixel_height
                    index = row * self.PIXEL_COLS + col
                    self.pixel_grid_canvas.coords(self.pixels[index], x1, y1, x2, y2)

    def _perform_redraw(self, new_canvas_width, new_canvas_height, current_pixel_size):
        # Only redraw if the pixel size has actually changed to a new integer value
        if current_pixel_size != self._last_pixel_size:
            self._last_pixel_size = current_pixel_size

            # Configure canvas size
            self.pixel_grid_canvas.place_configure(width=new_canvas_width, height=new_canvas_height)
            self._create_pixel_grid(new_canvas_width, new_canvas_height)
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

    def _drawFilledBlackBoxRoutine(self, x1, y1, x2, y2):
        """
        Fills a rectangular area with black pixels.
        (x1, y1) is the top-left corner, (x2, y2) is the bottom-right corner.
        """
        for y in range(y1, y2 + 1):
            for x in range(x1, x2 + 1):
                self.pixelOn(x, y)

    def _drawBlackBoxWithBordersRoutine(self, x1, y1, x2, y2):
        """
        Draws a stylized box with a black fill and a 1-pixel white border
        based on the provided Z80 routine logic.
        (x1, y1) is the top-left corner, (x2, y2) is the bottom-right corner
        of the overall black box.
        """
        # 1. Fill the entire box with black
        self._drawFilledBlackBoxRoutine(x1, y1, x2, y2)

        # 2. Calculate inner dimensions for border drawing (Z80 logic)
        # E = x2 - x1 - 4
        # D = y2 - y1 - 4
        # These represent the number of pixels to draw for the horizontal/vertical segments
        # of the inner border, excluding the corners.
        width_inner_segment = x2 - x1 - 4
        height_inner_segment = y2 - y1 - 4

        # If the box is too small to draw a border, return
        if width_inner_segment < 0 or height_inner_segment < 0:
            return

        # Initialize current drawing coordinates (L, H in Z80)
        current_x = x1
        current_y = y1

        # Z80: inc h, inc L, inc h, inc L
        current_y += 1 # H = y1 + 1
        current_x += 1 # L = x1 + 1
        current_y += 1 # H = y1 + 2
        current_x += 1 # L = x1 + 2
        # Current (x, y) is now (x1 + 2, y1 + 2)

        # Z80: call drawWhitePoint
        self.pixelOff(current_x, current_y) # Draws (x1+2, y1+2)

        # Z80: dec h
        current_y -= 1 # Current (x, y) is now (x1 + 2, y1 + 1)

        # Draw Top Border (Z80: ld b,e; loop call drawWhitePoint, inc L)
        for _ in range(width_inner_segment):
            self.pixelOff(current_x, current_y)
            current_x += 1

        # Z80: dec L
        current_x -= 1

        # Z80: inc H
        current_y += 1

        # Z80: call drawWhitePoint
        self.pixelOff(current_x, current_y) # Draws (x2-2, y1+2)

        # Z80: inc L
        current_x += 1 # Current (x, y) is now (x2 - 2, y1 + 2)

        # Draw Right Border (Z80: ld b,d; loop call drawWhitePoint, inc h)
        for _ in range(height_inner_segment):
            self.pixelOff(current_x, current_y)
            current_y += 1

        # Z80: dec h
        current_y -= 1

        # Z80: dec L
        current_x -= 1

        # Z80: call drawWhitePoint
        self.pixelOff(current_x, current_y) # Draws (x2-3, y2-2)

        # Z80: inc h
        current_y += 1 # Current (x, y) is now (x2 - 3, y2 - 2)

        # Draw Bottom Border (Z80: ld b,e; loop call drawWhitePoint, dec L)
        for _ in range(width_inner_segment):
            self.pixelOff(current_x, current_y)
            current_x -= 1

        # Z80: inc L
        current_x += 1

        # Z80: dec h
        current_y -= 1

        # Z80: call drawWhitePoint
        self.pixelOff(current_x, current_y) # Draws (x1+2, y2-3)

        # Z80: dec L
        current_x -= 1 # Current (x, y) is now (x1 + 1, y2 - 3)

        # Draw Left Border (Z80: ld b,d; loop call drawWhitePoint, dec h)
        for _ in range(height_inner_segment):
            self.pixelOff(current_x, current_y)
            current_y -= 1

    def parse_input(self, event=None):
        # Reset the entire pixel grid to black
        for row in range(self.PIXEL_ROWS):
            for col in range(self.PIXEL_COLS):
                self.pixelOn(col, row)

        input_text = self.text_input.get("1.0", tk.END).strip()
        #print(f"Input received: {input_text}") # For debugging
        tokenstream = parser.TokenStream.from_str(input_text)
        try:
            #TODO: Implement label and the two-pass system for forward-reference
            parseobj = parser.Parser(tokenstream, self.gamemap.symtable)
            datastream = parseobj.binres
        except Exception as e:
            datastream = e

        ptr = 0
        counter = 0
        EOF_encountered = None
        while ptr < len(datastream):
            counter += 1
            if counter > 1000:
                self.parser_status_label.config(text=f"Possible infinite loop detected. Halted.")
                return "break"
            opcode = datastream[ptr]
            if opcode == 0: # m_eof
                # Consumes 0 more bytes
                ptr += 1
                EOF_encountered = True
                break # End of stream
            elif opcode == 1: # m_setxy(x,y)
                # Consumes 2 more bytes (x, y)
                x = datastream[ptr + 1]
                y = datastream[ptr + 2]
                self.gamemap.x(x)
                self.gamemap.y(y)
                ptr += 3
            elif opcode == 2: # m_addxy(x,y)
                # Consumes 2 more bytes (x, y)
                x = datastream[ptr + 1]
                y = datastream[ptr + 2]
                # TODO: Implement addxy logic
                self.gamemap.x((self.gamemap.x()+x) & 0xFF)
                self.gamemap.y((self.gamemap.y()+y) & 0xFF)
                ptr += 3
            elif opcode == 3: # m_call(adr)
                # Consumes 2 more bytes (lo(adr), hi(adr))
                lo_adr = datastream[ptr + 1]
                hi_adr = datastream[ptr + 2]
                # TODO: Implement call logic
                ptr += 3
            elif opcode == 4: # m_print(adr)
                # Consumes 2 more bytes (lo(adr), hi(adr))
                lo_adr = datastream[ptr + 1]
                hi_adr = datastream[ptr + 2]
                # TODO: Implement print logic
                ptr += 3
            elif opcode == 5: # m_printacc(flags)
                # Consumes 1 more byte (flags)
                flags = datastream[ptr + 1]
                # TODO: Implement printacc logic
                ptr += 2
            elif opcode == 6: # m_jrfnz(flagid,rel)
                # Consumes 2 more bytes (flagid, rel)
                flagid = datastream[ptr + 1]
                rel = datastream[ptr + 2]
                # TODO: Implement jrfnz logic
                ptr += 3
            elif opcode == 7: # m_jrfz(flagid,rel)
                # Consumes 2 more bytes (flagid, rel)
                flagid = datastream[ptr + 1]
                rel = datastream[ptr + 2]
                # TODO: Implement jrfz logic
                ptr += 3
            elif opcode == 8: # m_drawbox(x1,y1,x2,y2)
                # Consumes 4 more bytes (x1, y1, x2, y2)
                x1 = datastream[ptr + 1]
                y1 = datastream[ptr + 2]
                x2 = datastream[ptr + 3]
                y2 = datastream[ptr + 4]
                self._drawBlackBoxWithBordersRoutine(x1, y1, x2, y2)
                self.gamemap.cr((self.gamemap.BML()+x1)&0xFF)
                self.gamemap.x(self.gamemap.cr)
                self.gamemap.y((self.gamemap.y()+self.gamemap.BMT())&0xFF)
                ptr += 5
            elif opcode == 9: # m_menuopt()
                # Consumes 0 more bytes
                # TODO: Implement menuopt logic
                ptr += 1
            elif opcode == 10: # m_newline()
                # Consumes 0 more bytes
                # TODO: Implement newline logic
                self.gamemap.x(self.gamemap.cr())
                self.gamemap.y((self.gamemap.NLH()+self.gamemap.y())&0x255)
                ptr += 1
            elif opcode == 11: # m_setflag(flagid)
                # Consumes 1 more byte (flagid)
                flagid = datastream[ptr + 1]
                # TODO: Implement setflag logic
                ptr += 2
            elif opcode == 12: # m_togflag(flagid)
                # Consumes 1 more byte (flagid)
                flagid = datastream[ptr + 1]
                # TODO: Implement togflag logic
                ptr += 2
            elif opcode == 13: # m_setleftmargin()
                # Consumes 0 more bytes
                # TODO: Implement setleftmargin logic
                self.gamemap.cr(self.gamemap.x())
                ptr += 1
            elif opcode == 14: # m_clracc()
                # Consumes 0 more bytes
                # TODO: Implement clracc logic
                self.gamemap.acc(0)
                ptr += 1
            elif opcode == 15: # m_exacc()
                # Consumes 0 more bytes
                # TODO: Implement exacc logic
                temp_acc = self.gamemap.acc()
                temp_shad = self.gamemap.accshad()
                self.gamemap.acc(temp_shad)
                self.gamemap.accshad(temp_acc)
                ptr += 1
            elif opcode == 16: # m_setacc1(val1b)
                # Consumes 1 more byte (val1b)
                val1b = datastream[ptr + 1]
                # TODO: Implement setacc1 logic
                ptr += 2
            elif opcode == 17: # m_setacc2(val2b)
                # Consumes 2 more bytes (lo(val2b), hi(val2b))
                lo_val2b = datastream[ptr + 1]
                hi_val2b = datastream[ptr + 2]
                # TODO: Implement setacc2 logic
                ptr += 3
            elif opcode == 18: # m_adrtoacc1(adr)
                # Consumes 2 more bytes (lo(adr), hi(adr))
                lo_adr = datastream[ptr + 1]
                hi_adr = datastream[ptr + 2]
                # TODO: Implement adrtoacc1 logic
                ptr += 3
            elif opcode == 19: # m_adrtoacc2(adr)
                # Consumes 2 more bytes (lo(adr), hi(adr))
                lo_adr = datastream[ptr + 1]
                hi_adr = datastream[ptr + 2]
                # TODO: Implement adrtoacc2 logic
                ptr += 3
            elif opcode == 20: # m_setidx(adr)
                # Consumes 2 more bytes (lo(adr), hi(adr))
                lo_adr = datastream[ptr + 1]
                hi_adr = datastream[ptr + 2]
                # TODO: Implement setidx logic
                ptr += 3
            elif opcode == 21: # m_addindexed(offset)
                # Consumes 1 more byte (offset)
                offset = datastream[ptr + 1]
                # TODO: Implement addindexed logic
                ptr += 2
            elif opcode == 22: # m_mltacc(val)
                # Consumes 1 more byte (val)
                val = datastream[ptr + 1]
                # TODO: Implement mltacc logic
                ptr += 2
            elif opcode == 23: # m_mltacc2(val)
                # Consumes 1 more byte (val)
                val = datastream[ptr + 1]
                # TODO: Implement mltacc2 logic
                ptr += 2
            else:
                x, _ = self.print_char(opcode, self.gamemap.x(), self.gamemap.y())
                self.gamemap.x(x)
                ptr += 1 # Advance to avoid infinite loop on unknown opcode

        if not EOF_encountered:
            self.parser_status_label.config(text=f"ERROR: STREAM WENT OOB WITHOUT EOF")
        else:
            self.parser_status_label.config(text=f"Emitted: '{datastream.hex()}'")
        return "break" # Prevents the default newline character from being inserted

if __name__ == "__main__":
    root = tk.Tk()
    app = MenuMakerApp(root)
    root.mainloop()
