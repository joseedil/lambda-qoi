# lambda-qoi

## Identification
José Edil G. de Medeiros
Brasília, Brazil

Video demo: https://youtu.be/ZOgjDTAnN8g

## Description:

The [Quite OK Image Format](https://qoiformat.org/) (QOI) is a lossless image compression algorithm developed by Dominic Szablewski. His goal was to achieve a fast codec without sacrificing too much the compression rates achieved by the PNG file format. `lambda-qoi` implements a transcoder from QOI to PNG and vice-versa, but the library is suficiently generic to allow one to decode a QOI File to a vector of pixels as part of another image processing application.

### Compilation and Use

Compile the project with `stack build`. It should be able to get the required libraries and build the project.

Run the executable with `stack exec lambda-qoi -- e <input.png> <output.qoi>` to encode a PNG image to a QOI file.

Run the executable with `stack exec lambda-qoi -- d <input.qoi> <output.png>` to decode a QOI image to a PNG file.

There are some test images in the `data` folder as provided by the QOI author.


### Implementation details

QOI is a simple file format with an [open-source specification](https://qoiformat.org/qoi-specification.pdf). The file consists of a 14-byte header, followed by data chunks and a 8-byte end marker. Decoding is achieved in a single pass, reading bytes from a data stream and deciding which kind of chunk it represents.

The file `src/Pixel.hs` has the basic data types to represent a pixel in the program. I'm using the pixel representations from the  [JuicyPixels](https://hackage.haskell.org/package/JuicyPixels-3.3.8) library to leverage that library capabilities of encoding and decoding PNG and other image file formats. Haskell has an interesting type system in which type clesses allow for highly polymorphic code. We define the `PixelDecode` type class to allow for decoding both RGB And RGBA images with the same function.

The header of the image files is defined in `src/Header.hs`. It consists of the image width and height info together with how many channels there are in each pixel and its colorspace. We use the [binary](https://hackage.haskell.org/package/binary) provides facilities to serialize and deserialize Haskell data types. Note that we treat the required header magic bytes in the `get` and `put` functions for the header, but we do not store that information, failing if we try to decode an invalid qoi file.

The `src/Util.hs` file defines some utility functions used in the decoder just to make the code a bit cleaner.

The main application lives in the `app/Main.hs` file. It reads the command line arguments, get the raw qoi encoded string from a file on the disk and tries to decode it, saving the decoded image to a PNG file on disk

#### Decoding

The file `src/Decoder.hs` is where the logic of the decoding algorithm lives. The type `Chunk` is used to represent the specified data chunks in which we can represent a single pixel, a run of repeated pixels and consult a pixel stored in a running array of previously seen pixels.

The `decodeChunk` function takes a byte string (the raw data read from a file or network stream), an index to the current byte being processed and the previsouly decoded pixel and returns a tuple with the number of consumed bytes in the byte string together with a data chunk. The implementation follows the specification, first trying to decode the 8-bit tag chunks or the 2-bit tagged ones.

The `decodeQoi` function implements the running loop that traverse the input data. It takes as argument the byte string to be decoded and the number of pixels in the image and returns a vector of pixels. Note that the output vector type is parameterized by our `PixelDecode` type class, allowing it to work with both RGB and RGBA internal pixel representations as long as we only use the functions of the type class.

This is an example of monadic code in Haskell. The `V.create` function takes a monadic action and freezes the resulting vector allowing us to use in-place mutation inside the function, but maintaining a pure external interface once the byte string is decoded. We start by creating a mutable vector `dataVec` to store the image pixels and a 64 byte `running` vector as required by the specification.

We define the local `updateRunning` function that updates the running vector with pixels as we decode them.

The local `loop` function is the workhorse. It takes an index to keep track of where we are specting the input byte string, another index to keep track of where we are storing in the output vector, and the previous decoded pixel. It tries to decode a new chunk from the byte string and process it accordingly.

- if we get a `One px`, just write the pixel in the output vector, update the running vector and recurse.
- if we get a `Lookback pos`, take the pixel from the running vector, write it to the output and recurse.
- if we get a `Repeat qty`, build a slice of the output vector, copy the previous pixel in all its elements, and recurse.

Finally, call the loop with initial values and return the output vector.

It follows two driving functions. `decodeQoiBS` takes a raw byte string and returns a tuple with the header and the decoded vector of pixels. The `decodeQoiPng` takes a raw bytestring and returns its header together with a `JuicyPixels` image representation that can be used to export the library supported file formats.

#### Encoding

The logic of the encoder lives in the file `src/Encoder.hs`. The functions `encodeDiff`, `encodeLuma`, `encodeIndex`, and `encodeRGB` try to create a QOI chunk from the information available. They can fail (return `Nothing`) in case of not being possible to create that kind of chunk. `encodeRGB` in particular handles both `RGB` and `RGBA` cases and can't fail (always emit a `Just` value). The `encodeRun` will always succeed to create a chunk.

The main driver is the `encodeData` function. It takes a `JuicyPixels` image and loops throught its pixels in raster scan order. In the first main branch it checks if the current pixel is equal to the previous visited pixel, in which case it manages the information required to emit a QOI_OP_RUN chunk. On the default branch the algorithm tries to emit the other chunks in the order that minimizes the final bitstream.

The `encodeImageRGB` and `encodeImageRGBA` functions build the final bitstream by building the header and end sequence.
