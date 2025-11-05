# Reads Modified Doors-CS Font source data into display arrays
# Doing this a rewrite because the original source is incomprehensible.


class Colors(object):
    RED = (255,0,0)
    WHITE = (255,255,255)
    BLACK = (0,0,0)



class DCFchar(object):
    ''' Builds the display array for the DCF glyph. Contents of `dataarray` is
        defined in list below, which changes depending on data structure.
        Character glyphs are bitpacked and always left-aligned (MSB-first).
        `displayname` is the name found in the comment of the source file.
        `longname`, if it exists, is the label used to point to the data in
        the wide character format.

        * Short - 3 bytes [WA BC DE], W=width <= 4, nibbles A,B,C,D,E
        * Long - 6 bytes [FW AA BB CC DD EE], F=(0xF), W=width, bytes A,B,C,D,E

        Class instances are intended to contain all the information needed to
        reconstruct the source character data, either visually, or as to dump
        it back into source format.
    '''
    def __init__(self, id, dataarray:list|bytearray, displayname, longname=None):
        #Binary digits are listed [76543210], 7=MSB, 0=LSB.
        self.id = id    #ASCII(-adjacent) character code.
        self.dispname = displayname
        self.longname = longname
        self.disparr = []
        dataiter = iter(dataarray)
        b0 = next(dataiter)
        if (b0 >> 4) == 0x0F:
            # This is a wide character if the top 4 bits is set.
            self.width = (b0 & 0x07)
            # Due to bit shenanigans, a width of 8 would render as 0 because
            # bit 4 is reserved for vertical shift, whereas a character that
            # is actually 0 pixels wide would instead be a thin character.
            # This problem never actually happens both before and with the
            # current EToR fontset. Best have a guard and document it, tho.
            if self.width == 0:
                self.width = 8
            self.verticalshift = True if b0 & 0b00001000 else False
            emptyrow = self.bin2arr(0, self.width)
            if self.verticalshift:
                self.disparr.append(emptyrow)
            # Leading row
            self.disparr.append(self.bin2arr((b0&0x0F)<<4, self.width))
            for byte in dataiter:
                self.disparr.append(self.bin2arr(byte&0xF0, self.width))
                self.disparr.append(self.bin2arr((byte<<4)&0xF0, self.width))
            if not self.verticalshift:
                self.disparr.append(emptyrow)
            pass
        else:
            # This is a thin character otherwise.
            self.width = (b0>>4) & 7    #bits 4,5,6 hold width up to 4.
            if self.width > 4:
                raise ValueError(f"Thin character width may not exceed 4.")
            self.verticalshift = True if b0 & 0b10000000 else False
            emptyrow = self.bin2arr(0, self.width)
            if self.verticalshift:
                self.disparr.append(emptyrow)
            # Leading row
            self.disparr.append(self.bin2arr((b0&0x0F)<<4, self.width))
            for byte in dataiter:
                self.disparr.append(self.bin2arr(byte&0xF0, self.width))
                self.disparr.append(self.bin2arr((byte<<4)&0xF0, self.width))
            if not self.verticalshift:
                self.disparr.append(emptyrow)
        return


    def bin2arr(self, bytedata:int, width:int):
        ''' Converts left-aligned `bytedata` data to array of `width` entries.
        '''
        if width == 0:
            return [Colors.WHITE]  #Something must be displayable, even if nothing.
        arr = []
        for idx in range(7,-1,-1):
            arr.append(Colors.BLACK if (1<<idx) & bytedata else Colors.WHITE)
        return arr



class DCFReader(object):
    def __init__(self, filename):
        pass











