import tkinter as tk
from tkinter import ttk
import dcfreader
import parser4 as parser
import os
import time
import inspect

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
    #include "src/xdefs/items.z80"
    #include "src/xdefs/spells.z80"
    #include "src/xdefs/players.z80"
    '''
    defaultsave = '''
    .org $C000  ;Placeholder location that's sufficiently out of the way
    #include "src/data/defaultsave.z80"
    '''
    def __init__(self):
        cls = self.__class__
        self.memory = [0] * 65536
        # Definitions
        tokens = parser.Tokenizer.from_str(cls.symfile)
        self.incparse = parser.Parser(tokens, silent=True)
        self.symtable = self.incparse.symtable
        #Default data
        tokens = parser.Tokenizer.from_str(cls.defaultsave)
        parserobj = parser.Parser(tokens, self.symtable, silent=True)
        segment = parserobj.curseg
        segorg = segment.baseaddr
        seglen = len(segment.data)
        self.memory[segorg:segorg+seglen] = list(segment.data)
        self.symtable = parserobj.symtable  #Updating symtable for new labels





    def get(self, sym, offset=0):
        return self.memory[int(self.symtable[sym].v)+offset]
    def set(self, sym, val, offset=0):
        self.memory[int(self.symtable[sym].v)+offset] = val&0xFF
    def get2(self, sym):
        adr = int(self.symtable[sym].v)
        return self.memory[adr] + (256* self.memory[adr+1])
    def set2(self, sym, val):
        adr = int(self.symtable[sym].v)
        self.memory[adr] = val & 0xFF
        self.memory[adr+1] = (val >> 8) & 0xFF
    def getval(self, label):
        obj = self.symtable[label]
        if isinstance(obj, parser.MacroDef):
            value = int(self.incparse.eval_expr(obj.body, 2).v)
        elif isinstance(obj, parser.Token):
            value = int(obj.v)
        else:
            raise ValueError(f"Object [{label}] of unknown type")
        return value
    def getaddr(self, label):
        return int(self.symtable[label].v)
    def exchange(self,f1,f2):
        a = f1()
        b = f2()
        f1(b)
        f2(a)
        return
    
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
    
    def idxshad(self, set=None):
        if set is not None:
            self.set2("textidxshadow", set)
        return self.get2("textidxshadow")

    def accshad(self, set=None):
        if set is not None:
            self.set2("textaccshadow", set)
        return self.get2("textaccshadow")
    def accshad2(self, set=None):
        if set is not None:
            self.set2("textaccshadow2", set)
        return self.get2("textaccshadow2")
    
    def NLH(self):
        return self.getval("NEWLINE_HEIGHT")
    
    def BMT(self):
        return self.getval("BOX_MARGIN_TOP")
    
    def BML(self):
        return self.getval("BOX_MARGIN_LEFT")
    
    def flags(self, offset, set=None):
        if set is not None:
            self.set("gameflags", set, offset)
        return self.get("gameflags", offset)


class FontReader(object):
    BIGBIGFONTDATA = '''bigBigFontData:
        ;00
        .db 7
        .db %01000100 ;FIRE
        .db %00010000
        .db %10110100
        .db %00101000
        .db %01001100
        .db %00111000
        .db %00000000
        ;01
        .db 5
        .db %00100000 ;LIT
        .db %01000000
        .db %11100000
        .db %01110000
        .db %00100000
        .db %01000000
        .db %00000000
        ;02
        .db 6
        .db %00000000 ;ICE
        .db %10101000
        .db %01110000
        .db %11011000
        .db %01110000
        .db %10101000
        .db %00000000
        ;03
        .db 6
        .db %01110000 ;POISON
        .db %10101000
        .db %10101000
        .db %01110000
        .db %01010000
        .db %00000000
        .db %00000000
        ;04
        .db 6
        .db %01111000 ;SLEEP
        .db %00010000
        .db %00100000
        .db %01111000
        .db %00000000
        .db %10000000
        .db %00000000
        ;05
        .db 7
        .db %01000000 ;SWORD
        .db %01100000
        .db %00110000
        .db %00010100
        .db %00001000
        .db %00010100
        .db %00000000
        ;06
        .db 7
        .db %00100000 ;STAFF
        .db %01110000
        .db %01010000
        .db %01011000
        .db %00001000
        .db %00000100
        .db %00000000
        ;07
        .db 7
        .db %00111000 ;SHIELD
        .db %01000100
        .db %01010100
        .db %01010100
        .db %01000100
        .db %00101000
        .db %00010000
        ;08
        .db 7
        .db %01101100 ;CLOTHING
        .db %01111100
        .db %00111000
        .db %00010000
        .db %00101000
        .db %00111000
        .db %00000000
        ;09
        .db 7
        .db %01101100 ;ARMOR
        .db %01010100
        .db %00111000
        .db %00101000
        .db %01010100
        .db %01101100
        .db %01000100
        ;10
        .db 7
        .db %00110000 ;ACCESSORY
        .db %01101000
        .db %01000000
        .db %01000000
        .db %01000000
        .db %01101000
        .db %00110000
        ;11
        .db 7
        .db %00111000 ;POTION
        .db %00010000
        .db %00010000
        .db %00101000
        .db %01000100
        .db %01111100
        .db %00000000
        ;12
        .db 7
        .db %01110000 ;KEYITEM
        .db %01000100
        .db %01111100
        .db %00100000
        .db %00110000
        .db %00100000
        .db %00110000
        ;13
        .db 5
        .db %00000000 ;HP
        .db %10010000
        .db %10010000
        .db %11110000
        .db %10010000
        .db %10010000
        .db %00000000
        ;14
        .db 6
        .db %00000000 ;MP
        .db %10001000
        .db %11011000
        .db %10101000
        .db %10001000
        .db %10001000
        .db %00000000
        ;15
        .db 7
        .db %10010000 ;(new wpn) CLAW
        .db %01001000
        .db %00100000 
        .db %10001100
        .db %01011100
        .db %00011000
        .db %00000000

        ;16
        ;.db %00000000
        ;.db %00000000
        ;.db %00000000
        ;.db %00000000
        ;.db %00000000
        ;.db %00000000
        ;.db %00000000
    '''
    def __init__(self, gamemap:GameMap):
        cls = self.__class__
        self.gamemap = gamemap
        self.dcf_reader = dcfreader.DCFReader("tools/escheron.dcf")
        tokens = parser.Tokenizer.from_str(cls.BIGBIGFONTDATA)
        self.parseobj = parser.Parser(tokens, silent=True)
        fontdata = self.parseobj.read_data()
        self.fontdata = [fontdata[i:i+8] for i in range(0, len(fontdata), 8)]
        for idx, data in enumerate(self.fontdata, start= int(self.gamemap.symtable["SYM_START"].body[0].v)):
            #print(f"{idx}: {data.hex()}")
            self.dcf_reader.update(dcfreader.BigBigchar(idx, data, str(idx)), idx)
        return

    def get_char(self, charid):
        return self.dcf_reader.get_char(charid)

class PortraitData(object):
    PORTRAIT_DATA = '''
        .db $11,$A2,$BC,$C0,$FC,$EC,$88,$80,$02,$3D,$05,$1C,$2D,$0C,$00,$00,$00,$80,$80,$80,$41,$20,$50,$4C,$01,$00,$C0,$02,$64,$06,$0E,$14
        .db $84,$18,$E1,$0E,$02,$FC,$14,$F0,$91,$A1,$B1,$AD,$61,$32,$02,$02,$20,$A0,$40,$40,$60,$70,$A8,$36,$02,$02,$0E,$70,$10,$20,$30,$28
        .db $86,$40,$E0,$3C,$6E,$18,$80,$82,$41,$02,$07,$3C,$2E,$58,$40,$41,$80,$02,$41,$C0,$60,$F3,$70,$38,$20,$41,$C1,$02,$02,$C7,$07,$0E
        .db $10,$C0,$80,$07,$3F,$E7,$7F,$3F,$B7,$B7,$83,$F0,$FE,$F3,$FF,$FE,$00,$22,$06,$00,$00,$C0,$E0,$70,$81,$BF,$BF,$BE,$BC,$BB,$B7,$A7
        .db $40,$80,$78,$F0,$D8,$B0,$80,$80,$09,$39,$FC,$6E,$1F,$03,$01,$01,$82,$43,$40,$40,$A3,$B0,$B9,$F4,$03,$03,$03,$06,$86,$0E,$1D,$3F
        .db $8E,$49,$40,$B0,$6C,$AF,$82,$8C,$59,$8A,$02,$8D,$B4,$F4,$40,$31,$04,$80,$60,$48,$48,$2B,$59,$0E,$20,$00,$03,$09,$09,$CA,$8D,$58
        .db $90,$3C,$42,$DC,$AC,$80,$80,$C0,$01,$1F,$21,$1F,$2D,$01,$01,$01,$C0,$C0,$60,$63,$71,$B8,$BC,$B7,$02,$82,$02,$C5,$8D,$1D,$2C,$DF
        .db $11,$13,$13,$77,$87,$3E,$FC,$FC,$F5,$F4,$F4,$F6,$F1,$1C,$0F,$0F,$FC,$FC,$FE,$FF,$3F,$87,$F7,$B3,$0F,$0F,$1F,$FE,$F9,$E7,$EA,$EB
    '''
    def __init__(self):
        cls = self.__class__
        tokens = parser.Tokenizer.from_str(cls.PORTRAIT_DATA)
        self.parseobj = parser.Parser(tokens, silent=True)
        imgdata = self.parseobj.read_data()
        imgdata = [self.bin2arr(i, 8) for i in imgdata]
        listing = [imgdata[i:i+8] for i in range(0, len(imgdata), 8)]
        self.imgdata = [listing[i:i+4] for i in range(0, len(listing), 4)]

    def bin2arr(self, bytedata:int, width:int):
        if width == 0:
            return [dcfreader.Colors.WHITE]  #Something must be displayable, even if nothing.
        arr = []
        for idx in range(7,7-width,-1):
            arr.append(dcfreader.Colors.BLACK if (1<<idx) & bytedata else dcfreader.Colors.WHITE)
        return arr


class MenuMakerApp:
    PIXEL_COLS = 96
    PIXEL_ROWS = 64
    PIXEL_ON_COLOR = "black"
    PIXEL_OFF_COLOR = "white"

    # Display flags
    dhl_show_lead_zeroes  = 3
    dhl_right_align_num   = 4
    dhl_indic_neg_sign    = 5
    dhl_show_pos_sign     = 6
    dhl_fetch_long_values = 7

    mf_SHOW_LEAD_ZEROES = (1 << dhl_show_lead_zeroes)
    mf_RIGHT_ALIGN = (1 << dhl_right_align_num)
    mf_SIGNED_SHOW_NEG = (1 << dhl_indic_neg_sign)
    mf_SIGNED_SHOW_POS = (1 << dhl_show_pos_sign)
    mf_USE_LONG_VALUES = (1 << dhl_fetch_long_values)

    SHOW_PERFORMANCE_METRICS = False

    def __init__(self, master:tk.Tk):
        self.gamemap = GameMap()
        self.master = master
        self.interim_parseobj = None
        self.update_interval = 0
        self.dcf_reader = FontReader(self.gamemap)
        self.portraits = PortraitData()
        master.title("Application Title")
        master.state('zoomed')
        master.after(1000, self.autoupdater)
        # Run-time binding symbol name to function dict
        self.callbind = dict()
        self.callbind['menu_drawPortrait'] = lambda: self.render_portrait(self.gamemap.acc())
        self.callbind['menu_drawBox'] = lambda x1,y1,x2,y2: self.menusystem_render_box(x1,y1,x2,y2)


        self.input_history_file = "tools/menumaker_input.txt"
        master.protocol("WM_DELETE_WINDOW", self._on_closing) # Handle window close event

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

        self._load_input_history() # Load input history on startup

        # Bind F5 key to parse_input method
        self.text_input.bind("<F5>", self._reparse)
        self.master.bind_all("<F6>", self._reinit_and_reparse)
        self.master.bind_all("<F8>", self._debug_reparse)

    def autoupdater(self, *args, **kwargs):
        if self.update_interval:
            interval = self.update_interval
            self.parse_input()
        else:
            interval = 1000
        self.master.after(interval, self.autoupdater)

    def _debug_reparse(self, *args, **kwargs):
        time_start = time.time()
        self.parse_input(*args, showsym=True, **kwargs)
        if self.__class__.SHOW_PERFORMANCE_METRICS:
            print(f"Time to execute reparse: {time.time()-time_start}")

    def _reparse(self, *args, **kwargs):
        time_start = time.time()
        self.parse_input(*args, **kwargs)
        if self.__class__.SHOW_PERFORMANCE_METRICS:
            print(f"Time to execute reparse: {time.time()-time_start}")


    def _reinit_and_reparse(self, *args, **kwargs):
        time_start = time.time()
        self.gamemap = GameMap()
        self.parse_input()
        if self.__class__.SHOW_PERFORMANCE_METRICS:
            print(f"Time to execute reinit and reparse: {time.time()-time_start}")

    def _on_closing(self):
        """Handles the window closing event, saving the input history."""
        self._save_input_history()
        self.master.destroy()

    def _save_input_history(self):
        """Saves the current content of the text input field to a file."""
        content = self.text_input.get("1.0", tk.END).strip()
        try:
            with open(self.input_history_file, "w", encoding="utf-8") as f:
                f.write(content)
        except IOError as e:
            print(f"Error saving input history: {e}")

    def _load_input_history(self):
        """Loads the input history from a file and inserts it into the text input field."""
        if os.path.exists(self.input_history_file):
            try:
                with open(self.input_history_file, "r", encoding="utf-8") as f:
                    content = f.read()
                    self.text_input.delete("1.0", tk.END)
                    self.text_input.insert("1.0", content)
            except IOError as e:
                print(f"Error loading input history: {e}")

    def _print_number(self, number, flags):
        # Extract digit limit (first three bits)
        digit_limit = flags & 0b00000111 # mf_1DIGIT to mf_7DIGIT, mf_8DIGIT is 0

        # Determine signedness
        is_signed = (flags & self.mf_SIGNED_SHOW_NEG) or (flags & self.mf_SIGNED_SHOW_POS)

        # Convert 16-bit unsigned to signed if necessary for internal logic
        original_number = number
        if is_signed and (number & 0x8000):
            original_number = -(0x10000 - number)

        # Calculate max/min values based on digit_limit
        max_val = (10 ** digit_limit) - 1 if digit_limit > 0 else 0
        min_val = -(10 ** digit_limit) + 1 if digit_limit > 0 else 0 # For signed, e.g., -99 for 2 digits
        #print(f"Given {number} flags {flags}, min: {min_val}, max: {max_val}")

        # Clamp the number
        if is_signed:
            if original_number > max_val:
                number_to_print = max_val
            elif original_number < min_val:
                number_to_print = min_val
            else:
                number_to_print = original_number
        else: # Unsigned
            if original_number < 0: # Unsigned numbers cannot be negative
                number_to_print = 0
            elif original_number > max_val:
                number_to_print = max_val
            else:
                number_to_print = original_number

        # Format the number string
        num_str = str(abs(number_to_print)) # Start with absolute value for padding/zfill

        # Apply leading zeroes
        if flags & self.mf_SHOW_LEAD_ZEROES:
            num_str = num_str.zfill(digit_limit)

        # Add sign if necessary
        if is_signed:
            if number_to_print < 0:
                num_str = "-" + num_str
            elif number_to_print > 0 and (flags & self.mf_SIGNED_SHOW_POS):
                num_str = "+" + num_str
        
        # Handle right alignment
        if flags & self.mf_RIGHT_ALIGN:
            # The total width for right alignment should account for the digit limit
            # plus one for the sign if present.
            actual_display_width = digit_limit
            if is_signed and (number_to_print < 0 or (number_to_print > 0 and (flags & self.mf_SIGNED_SHOW_POS))):
                actual_display_width += 1 # Account for the sign character

            # Pad with spaces to the calculated display width
            num_str = num_str.rjust(actual_display_width)
            # Space-doubling because spaces in this font is only half a number's width
            num_str = ''.join(["  " if i == ' ' else i for i in num_str])


        # Print the formatted string
        current_x = self.gamemap.x()
        current_y = self.gamemap.y()
        for char_code in num_str:
            current_x, current_y = self.print_char(ord(char_code), current_x, current_y)
        self.gamemap.x(current_x) # Update gamemap's x position

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

    def render_portrait(self, portraitid:int):
        imgdata = self.portraits.imgdata[portraitid]
        basex = self.gamemap.x()
        basey = self.gamemap.y()
        for tiley in range(2):
            for tilex in range(2):
                sprite = imgdata[tilex+2*tiley]
                for spry, rowdata in enumerate(sprite):
                    for sprx, pixel in enumerate(rowdata):
                        tx = basex + 8*tilex + sprx
                        ty = basey + 8*tiley + spry
                        try:
                            if pixel == dcfreader.Colors.BLACK:
                                self.pixelOn(tx, ty)
                            else:
                                self.pixelOff(tx, ty)
                        except:
                            pass

    def menusystem_render_box(self, x1, y1, x2, y2):
        self._drawBlackBoxWithBordersRoutine(x1, y1, x2, y2)
        self.gamemap.cr((self.gamemap.BML()+x1)&0xFF)
        self.gamemap.x(self.gamemap.cr())
        self.gamemap.y((y1+self.gamemap.BMT())&0xFF)

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
        x2 = x2 - 1
        y2 = y2 - 1
        self._drawFilledBlackBoxRoutine(x1, y1, x2, y2)

        # 2. Calculate inner dimensions for border drawing (Z80 logic)
        # E = x2 - x1 - 4
        # D = y2 - y1 - 4
        # These represent the number of pixels to draw for the horizontal/vertical segments
        # of the inner border, excluding the corners.
        width_inner_segment = x2 - x1 - 4 + 1
        height_inner_segment = y2 - y1 - 4 + 1

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

    def parse_input(self, event=None, showsym=False):
        # Reset the entire pixel grid to black
        for row in range(self.PIXEL_ROWS):
            for col in range(self.PIXEL_COLS):
                self.pixelOn(col, row)

        input_text = self.text_input.get("1.0", tk.END).strip()
        #print(f"Input received: {input_text}") # For debugging
        tokenstream = parser.Tokenizer.from_str(input_text)
        fullcallbind = dict()
        try:
            exception = None
            parseobj = parser.Parser(tokenstream, self.gamemap.symtable, silent=True)
            self.interim_parseobj = parseobj
            segment:parser.SegmentDef = parseobj.segments["__DEFAULT"]
            origin = segment.baseaddr
            data = segment.data
            #NOTE: fullcallbind relates assembly address to label name. Then you
            #   use that label as a key in self.callbind to call routine
            for key in self.callbind:
                if key in parseobj.symtable:
                    try:
                        val = int(parseobj.symtable[key].v)
                        if val in fullcallbind:
                            print(f"Labels must have unique values/addresses. Found redefinition of '{fullcallbind[val]}' in '{key}', with value: {val} ")
                        else:
                            fullcallbind[val] = key
                    except:
                        continue
            if "__REPARSE_INTERVAL"  in parseobj.symtable:
                try:
                    newinter = int(parseobj.symtable["__REPARSE_INTERVAL"].body[0].v)
                    if newinter > 100 and newinter < 5000:
                        self.update_interval = newinter
                except Exception as e:
                    print(e)
                    pass
                pass
            else:
                self.update_interval = 0
            if showsym:
                parseobj.printsym()
        except Exception as e:
            self.interim_parseobj = None
            exception = e
            origin = 0
            data = bytearray([0])

        self.gamemap.memory[origin:origin+len(data)] = data
        if len(self.gamemap.memory) != 65536:
            raise ValueError("BAD OBJECT REPLACEMENT IN GAMEMAP MEMORY.")
        ptr = origin
        counter = 0
        bound_start = origin
        bound_end = origin+len(data)
        EOF_encountered = None
        print_stack = []
        print("---")
        while True:
            if EOF_encountered:
                if len(print_stack):
                    ptr = print_stack.pop()
                    EOF_encountered = False
                    continue
                if exception:
                    self.parser_status_label.config(text=f"Exc: {exception}")
                else:
                    self.parser_status_label.config(text=f"{len(data)} bytes at 0x{origin:04x} ran successfully.")
                break
            if (ptr < bound_start or ptr >= bound_end) or EOF_encountered:
                if exception:
                    self.parser_status_label.config(text=f"Exc: {exception}")
                else:
                    self.parser_status_label.config(text=f"ERR: Stream exited bounds without EOF.")
                break
            counter += 1
            if counter > 1000:
                self.parser_status_label.config(text=f"Possible infinite loop detected. Halted.")
                return "break"
            opcode = self.gamemap.memory[ptr]
            if opcode == 0:     # 1b: m_eof
                ptr += 1
                EOF_encountered = True
                continue        # Go back to EOF handler.
            elif opcode == 1:   # 3b: m_setxy(x,y)
                self.gamemap.x(self.gamemap.memory[ptr + 1])
                self.gamemap.y(self.gamemap.memory[ptr + 2])
                ptr += 3
            elif opcode == 2:   # 3b: m_addxy(x,y)
                x = self.gamemap.memory[ptr + 1]
                y = self.gamemap.memory[ptr + 2]
                self.gamemap.x((self.gamemap.x()+x) & 0xFF)
                self.gamemap.y((self.gamemap.y()+y) & 0xFF)
                ptr += 3
            elif opcode == 3:   #  obsoleted
                ptr += 1
            elif opcode == 4:   # 2b: m_dispacc() and overloaded company
                flags = self.gamemap.memory[ptr + 1]
                if (flags&0x07) in (6, 7, 0):
                    oper = flags & 0x07
                    num = (flags >> 3) & 0x1F
                    num = num if num<16 else num-32
                    if oper == 6:
                        self.gamemap.acc(self.gamemap.acc()+num)
                    elif oper == 7:
                        self.gamemap.idx(self.gamemap.idx()+num)
                    elif oper == 0:
                        self.gamemap.accshad(self.gamemap.accshad()+num)
                else:
                    self._print_number(self.gamemap.acc(), flags)
                ptr += 2
            elif opcode == 5:   # obsoleted
                ptr += 1
            elif opcode == 6:   # 3(+n)b: m_run(adr)
                #NOTE: Supports reading args inline with text engine.
                #   One data byte per argument, arguments determined by
                #   function signature. In the assembly, the callee is 
                #   what will manipulate the stack frame to make this so.
                lo_adr = self.gamemap.memory[ptr + 1]
                hi_adr = self.gamemap.memory[ptr + 2]
                adr = (hi_adr*256)+lo_adr
                fargs = []
                if adr in fullcallbind:
                    k = fullcallbind[adr]
                    if k in self.callbind:
                        f = self.callbind[k]
                        fsig = inspect.signature(f)
                        fargs = [0] * len(fsig.parameters)
                        for i in range(len(fargs)):
                            fargs[i] = self.gamemap.memory[ptr + 3 + i]
                        #print(f"Runcode {adr}:{k}, args: {fargs}, function: {f}")
                        f(*fargs)
                ptr += (3+len(fargs))
            elif opcode == 7:   # 1b: m_clra0()
                self.gamemap.acc1(0)
                ptr += 1
            elif opcode == 8:   # 2b: m_seta0(v)
                self.gamemap.acc1(self.gamemap.memory[ptr + 1])
                ptr += 2
            elif opcode == 9:   # 1b: m_menuopt()
                #raise NotImplementedError("OPCODE 9 requires that I have an actual menu system.")
                ptr += 1        # TODO: Implement menuopt logic
            elif opcode == 10:  # 1b: m_newline()
                self.gamemap.x(self.gamemap.cr())
                self.gamemap.y((self.gamemap.NLH()+self.gamemap.y())&0xFF)
                ptr += 1
            elif opcode == 11:  # 1b: m_swap()
                a,b = self.gamemap.acc().to_bytes(2, "little")
                self.gamemap.acc(a*256+b)
                ptr += 1
            elif opcode == 12:  # 1b: m_exaap()
                self.gamemap.exchange(self.gamemap.acc, self.gamemap.accshad)
                ptr += 1
            elif opcode == 13:  # 1b: m_setleft() : Sets left margin via curx.
                self.gamemap.cr(self.gamemap.x())
                ptr += 1
            elif opcode == 14:  # 1b: m_exiip()
                self.gamemap.exchange(self.gamemap.idx, self.gamemap.idxshad)
                ptr += 1
            elif opcode == 15:  # 1b: m_exai()
                self.gamemap.exchange(self.gamemap.acc, self.gamemap.idx)
                ptr += 1
            elif opcode == 16:  # 1b: m_addap()
                v = (self.gamemap.acc()+self.gamemap.accshad())&0xFFFF
                self.gamemap.acc(v)
                ptr += 1
            elif opcode == 17:  # 1b: m_addi()
                v = (self.gamemap.acc()+self.gamemap.idx())&0xFFFF
                self.gamemap.acc(v)
                ptr += 1
            elif opcode == 18:  # 1b: m_sext()  : Signextends a0 into a1
                # Consumes 2 more bytes (lo(adr), hi(adr))
                v = self.gamemap.acc1()
                v = v if not (v & 0x80) else v | 0xFF00
                self.gamemap.acc(v)
                ptr += 1
            elif opcode == 19:  # 2b: m_addind(v)
                offset = self.gamemap.memory[ptr + 1]
                offset_int8 = offset if offset < 128 else offset-256
                memlookup = self.gamemap.memory[self.gamemap.idx()+offset_int8]
                self.gamemap.acc1(self.gamemap.acc1()+memlookup)
                ptr += 2
            elif opcode == 20:  # 1b: m_mltacc()
                a, b = self.gamemap.acc().to_bytes(2, "little")
                self.gamemap.acc(a*b)
                ptr += 1
            elif opcode == 21:  # 2b: m_djnz(rel)
                self.gamemap.acc((self.gamemap.acc()-1)&0xFFFF)
                v = self.gamemap.acc() & 0xFF
                rel = self.gamemap.memory[ptr + 1]
                ptr += 2
                if v != 0:
                    ptr += rel if rel < 128 else rel-256
            elif opcode == 22:  # 1b: m_write()  : a0 -> [I]
                adr = self.gamemap.idx()
                self.gamemap.memory[adr] = self.gamemap.acc1()
                ptr += 1
            elif opcode == 23:  # 3b: multi-use jump instruction
                param = self.gamemap.memory[ptr + 1]
                b = param & 0x0F
                topoper = (param & 0xC0) >> 6
                oper = (param & 0x30) >> 4
                rel = self.gamemap.memory[ptr + 2]
                if topoper == 0:    #JRB
                    if oper == 0:
                        res = True if (self.gamemap.acc() & (1 << b)) == 0 else False
                    elif oper == 1:
                        res = True if (self.gamemap.acc() & (1 << b)) != 0 else False
                    elif oper == 2:
                        res = True if (self.gamemap.accshad() & (1 << b)) == 0 else False
                    else:
                        res = True if (self.gamemap.accshad() & (1 << b)) != 0 else False
                    ptr += 3
                    if res:
                        ptr += (rel if rel < 128 else rel-256)
                elif topoper == 1:  #JR unconditional
                    res = True
                    rel = rel|((param & 0x3F) << 8)
                    #print(f"JR OLD PTR: {ptr}, rel {rel}")
                    ptr += 3 + (rel if rel < 8096 else rel-16384)
                    #print(f"JR NES PTR: {ptr}")
                elif topoper == 2:  #JSR/ relative call
                    print_stack.append(ptr + 3)
                    ptr += 3 + (rel if rel < 8096 else rel-16384)
                    continue
                else: #JSR@reg / abs call from reg. No rel supplied
                    if oper == 0:
                        v = self.gamemap.acc()
                    elif oper == 1:
                        v = self.gamemap.accshad()
                    elif oper == 2:
                        v = self.gamemap.idx()
                    else:
                        v = self.gamemap.idxshad()
                    print_stack.append(ptr + 2)
                    ptr = v
                    continue
            elif opcode == 24:  # obsoleted
                ptr += 1
            elif opcode == 25:  # 2b: m_exfa()/m_ldfa()   ;%SSCAAAAA. Size indicates power of two here.
                bytecode = self.gamemap.memory[ptr + 1]
                #print(f"Bytecode renders: {bytecode.to_bytes(1,'big').hex()}")
                bitcodesize = ((bytecode >> 6) & 3)
                size = 1 << bitcodesize
                offset = bytecode & 31
                oper = (bytecode >> 5) & 1
                flagbank = self.gamemap.getaddr("gameflags")
                regframe = self.gamemap.getaddr("textacc")
                #print(f"Size: {size}: orig: {bitcodesize}")
                for idx in range(size):
                    a = self.gamemap.memory[regframe]
                    if oper == 0:
                        # Exchange A <-> [mem]
                        #print(f"EXCH {a} <-> {self.gamemap.memory[flagbank+offset]}")
                        self.gamemap.memory[regframe] = self.gamemap.memory[flagbank+offset]
                        self.gamemap.memory[flagbank+offset] = a
                    else:
                        # Mov A -> [mem]
                        #print(f"LOAD {a} <-> {self.gamemap.memory[flagbank+offset]}")
                        self.gamemap.memory[flagbank+offset] = a
                    flagbank += 1
                    regframe += 1
                ptr += 2
            elif opcode == 26:  #2b m_Xaf(). %OOOSAAAA. Massively overloaded. I guess size would be power of two here too.
                bytecode = self.gamemap.memory[ptr + 1]
                is2byte = False if ((bytecode >> 4) & 1) == 0 else True
                offset = bytecode & 15
                oper = (bytecode >> 5) & 7
                acc = self.gamemap.acc()
                flagbank = self.gamemap.getaddr("gameflags")
                other = self.gamemap.memory[flagbank + offset]
                if is2byte:
                    other += (self.gamemap.memory[flagbank + offset + 1] << 8)
                else:
                    acc = acc & 0xFF
                if oper == 0:
                    acc = other
                elif oper == 1:
                    acc = acc | other
                elif oper == 2:
                    acc = acc & other
                elif oper == 3:
                    acc = acc ^ other
                elif oper == 4:
                    acc = acc + other
                elif oper == 5:
                    acc = acc - other
                elif oper == 6:
                    acc = (-other) & 0xFFFF
                elif oper == 7:
                    acc = (~other) & 0xFFFF
                self.gamemap.acc(acc)
                ptr += 2
            elif opcode == 27:  # 2b: m_ldind()
                offset = self.gamemap.memory[ptr + 1]
                offset_int8 = offset if offset < 128 else offset-256
                memlookup = self.gamemap.memory[self.gamemap.idx()+offset_int8]
                self.gamemap.acc(memlookup & 0xFF)
                ptr += 2
            elif opcode == 28:  # 3b: cpjnz() compare val with a0, jump if not equal
                arg1 = self.gamemap.acc() & 0xFF
                arg2 = self.gamemap.memory[ptr + 1]
                rel = self.gamemap.memory[ptr + 2]
                ptr += 3
                if arg1 != arg2:
                    ptr += rel if rel < 128 else rel-256
            elif opcode == 31:  # 1b: m_debug  : prints debug info to console
                a = self.gamemap.acc().to_bytes(2, 'big').hex()
                ap = self.gamemap.accshad().to_bytes(2, 'big').hex()
                i = self.gamemap.idx().to_bytes(2, 'big').hex()
                ip = self.gamemap.idxshad().to_bytes(2, 'big').hex()
                ap2 = self.gamemap.accshad2().to_bytes(2, 'big').hex()
                flagadr = self.gamemap.getaddr("gameflags")
                hilite = 7*2
                flagstring = bytes(self.gamemap.memory[flagadr:flagadr+16]).hex()
                tempstr = parser.yellowmsg(flagstring[hilite:hilite+4])
                flagstring = flagstring[:hilite]+tempstr+flagstring[hilite+4:]
                print(f"A: ${a}, AP: ${ap}, I: ${i}, IP: ${ip}, FMEM: {flagstring}")
                ptr += 1
            else:
                nx, _ = self.print_char(opcode, self.gamemap.x(), self.gamemap.y())
                self.gamemap.x(nx)
                ptr += 1 # Advance to avoid infinite loop on unknown opcode
        return "break" # Prevents the default newline character from being inserted

if __name__ == "__main__":
    root = tk.Tk()
    app = MenuMakerApp(root)
    root.mainloop()
