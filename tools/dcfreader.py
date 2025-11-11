# Reads Modified Doors-CS Font source data into display arrays
# Doing this a rewrite because the original source is incomprehensible.
from typing import NamedTuple, Optional


class Colors(object):
    # These constants lie. But for good reason. We're predominantly printing
    # onto a black background using white letters, so it has to be backwards.
    # I have no good way to specify that the printing should be done via XOR
    # and not have the printer do uncomfortably hideous logic
    RED = (255,0,0)
    WHITE = (0,0,0)
    BLACK = (255,255,255)



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
            self.width = (b0>>4) & 7    #bits 4,5,6 hold width up to 5.
            # NOTE: Max width is 5 for thin, not 4, because we also encode the 
            # single pixel whitespace after the end of the glyph. Since a nibble
            # is only 4 pixels wide, specifying an empty bit is unnecessary.
            if self.width > 5:
                raise ValueError(f"Thin character width may not exceed 5.")
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
        for idx in range(7,7-width,-1):
            arr.append(Colors.BLACK if (1<<idx) & bytedata else Colors.WHITE)
        return arr


class DCFDefer(NamedTuple):
    id:int
    data:list
    name:str
    label:str

class DCFReader(object):
    def __init__(self, filename):
        ''' NOTE: Most of the splits and program flows was due to staring at
            the source and making observations about them. These observations
            are poorly recorded here. I knew this the last time, and they
            weren't recorded AT ALL back then. Future-You will have to suffer
            staring at the .DCF file as well. I am less sorry than I was then.
        '''
        filedata = None
        with open(filename) as f:
            filedata = f.readlines()
        if not filedata:
            raise ValueError(f"File {filename} empty or inaccessable.")
        
        self.defaultchar = DCFchar(-1,[0,0,0], "DEFAULTCHAR", "DEFAULTCHAR")
        
        codepoints:Optional[DCFchar] = [None]*256
        deferrals:list[DCFDefer] = []
        deferrallabels:list[str] = []
        # This assumes that the table defines all 32 control codes, and so the
        # first 32 lines are dedicated to them. We get around this by commenting
        # them out. The index continues to rise while nothing is emitted. This
        # is desirable.
        for index, line in enumerate(filedata):
            line = line.strip()
            if not line or line.startswith(';'):
                continue
            data,comment = line.split(';')
            data = data.split()
            #NOTE: All commented out lines were filtered out at this point.
            # The only other lines that don't start with .db are the lines
            # with labels on them. That is, the continuation of wide-char data.
            if not data[0].lower() == '.db':
                if data[0] in deferrallabels:
                    deferidx = deferrallabels.index(data[0])
                    _ = deferrallabels.pop(deferidx)
                    deferral = deferrals.pop(deferidx)
                    bytedata = deferral.data
                    bytedata.extend(self.text2bin(data[2]))
                    #print(f"[{deferral.id}]: {bytedata}, {deferral.name}")
                    c = DCFchar(deferral.id, bytedata, deferral.name, deferral.label)
                    codepoints[index] = c
                continue
            # Otherwise, continue processing glyph data.
            comment = comment.split()
            bytedata = self.text2bin(data[1])  #The stuff after the first '.db'
            if ".dw" in data:
                deferral = DCFDefer(index, bytedata, comment[0], data[4])
                deferrals.append(deferral)
                deferrallabels.append(deferral.label)

                continue
            #print(f"[{index}]: {bytedata}, {comment[0]}")
            c = DCFchar(index, bytedata, comment[0], comment[0])
            codepoints[index] = c
        self.codepoints = codepoints

    def text2bin(self, text:str) -> list:
        items = text.split(',')
        arr = []
        for item in items:
            if not item.startswith("$"):
                raise ValueError("Expected numeric value not formatted hexadecimal.")
            arr.append(int(item[1:],16))
        return arr
    
    def get_char(self, charid:int) -> DCFchar:
        try:
            char = self.codepoints[charid]
        except:
            char = self.defaultchar
        return char



if __name__ == '__main__':
    data = DCFReader("tools/escheron.dcf")
    pass

