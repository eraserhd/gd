module Graphics.GD.ByteString (
                    -- * Types
                    Image, Size, Point, Color,
                    -- * Creating and copying images
                    newImage, copyImage, 
                    copyRegion, copyRegionScaled,
                    -- * Memory management
                    withImage,
                    -- * Loading images
                    -- ** JPEG
                    loadJpegFile, loadJpegData, loadJpegByteString,
                    -- ** PNG
                    loadPngFile, loadPngData, loadPngByteString,
                    -- ** GIF
                    loadGifFile, loadGifData, loadGifByteString,
                    -- * Saving images
                    -- ** JPEG
                    saveJpegFile, saveJpegByteString,
                    -- ** PNG
                    savePngFile, savePngByteString,
                    -- ** GIF
                    saveGifFile, saveGifByteString,
                    -- * Getting image information
                    imageSize,
                    -- * Querying
                    getPixel,
                    -- * Manipulating images
                    resizeImage, rotateImage,
                    -- * Drawing
                    fillImage,
                    drawFilledRectangle,
                    drawFilledEllipse,
                    drawLine,
                    drawArc,
                    antiAliased,
                    setPixel,
                    -- * Text
                    useFontConfig,
                    drawString,
                    measureString,
                    drawStringCircle,
                    -- * Colors
                    rgb, rgba
                   ) where

import Graphics.GD.Internal

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

import Control.Monad (liftM,unless)
import Foreign
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Error

--
-- * Loading images
--

-- | Load a JPEG image from a file.
loadJpegFile :: FilePath -> IO Image
loadJpegFile = loadImageFile gdImageCreateFromJpeg

-- | Load a JPEG image from a buffer.
loadJpegData :: Int -- ^ Buffer size.
             -> Ptr a -- ^ Buffer with image data.
             -> IO Image
loadJpegData = loadImageData gdImageCreateFromJpegPtr

-- | Load a JPEG image from a ByteString
loadJpegByteString :: B.ByteString -> IO Image
loadJpegByteString = onByteStringData loadJpegData


-- | Load a PNG image from a file.
loadPngFile :: FilePath -> IO Image
loadPngFile = loadImageFile gdImageCreateFromPng

-- | Load a PNG image from a buffer.
loadPngData :: Int -- ^ Buffer size.
            -> Ptr a -- ^ Buffer with image data.
            -> IO Image
loadPngData = loadImageData gdImageCreateFromPngPtr

-- | Load a PNG image from a ByteString
loadPngByteString :: B.ByteString -> IO Image
loadPngByteString = onByteStringData loadPngData

-- | Load a GIF image from a file.
loadGifFile :: FilePath -> IO Image
loadGifFile = loadImageFile gdImageCreateFromGif

-- | Load a GIF image from a buffer.
loadGifData :: Int -- ^ Buffer size.
            -> Ptr a -- ^ Buffer with image data.
            -> IO Image
loadGifData = loadImageData gdImageCreateFromGifPtr

-- | Load a GIF image from a ByteString
loadGifByteString :: B.ByteString -> IO Image
loadGifByteString = onByteStringData loadGifData


loadImageFile :: (Ptr CFILE -> IO (Ptr GDImage)) -> FilePath -> IO Image
loadImageFile f file = 
    do p <- throwIfNull ("Loading image from " ++ file) $ withCFILE file "rb" f
       mkImage p

loadImageData :: (CInt -> Ptr a -> IO (Ptr GDImage)) -> Int -> Ptr a -> IO Image
loadImageData f sz buf =
    do p <- throwIfNull ("Loading image") $ f (fromIntegral sz) buf
       mkImage p

onByteStringData :: (Int -> Ptr a -> IO b) -> B.ByteString -> IO b
onByteStringData f bstr 
    = case BI.toForeignPtr bstr of
        (fptr, start, sz) -> withForeignPtr fptr (\ptr -> f sz (plusPtr ptr start))

--
-- * Saving images
--

-- | Save an image as a JPEG file.
saveJpegFile :: Int -- ^ quality: 0-95, or negative for default quality.
             -> FilePath -> Image -> IO ()
saveJpegFile q = saveImageFile (\p h -> gdImageJpeg p h (fromIntegral q))

-- | Write a JPEG format ByteString of an image.
saveJpegByteString :: Int -> Image -> IO B.ByteString
saveJpegByteString q = saveImageByteString (\p h -> gdImageJpegPtr p h (fromIntegral q))


-- | Save an image as a PNG file.
savePngFile :: FilePath -> Image -> IO ()
savePngFile = saveImageFile gdImagePng

-- | Write a PNG format ByteString of an image.
savePngByteString :: Image -> IO B.ByteString
savePngByteString = saveImageByteString gdImagePngPtr


-- | Save an image as a GIF file.
saveGifFile :: FilePath -> Image -> IO ()
saveGifFile = saveImageFile gdImageGif

-- | Write a GIF format ByteString of an image.
saveGifByteString :: Image -> IO B.ByteString
saveGifByteString = saveImageByteString gdImageGifPtr

saveImageFile :: (Ptr GDImage -> Ptr CFILE -> IO ()) -> FilePath -> Image -> IO ()
saveImageFile f file i = withImagePtr i (\p -> withCFILE file "wb" (f p))

saveImageByteString :: (Ptr GDImage -> Ptr CInt -> IO (Ptr a)) -> Image -> IO (B.ByteString)
saveImageByteString f img = withImagePtr img (\p -> dataByteString (f p))

dataByteString :: (Ptr CInt -> IO (Ptr a)) -> IO B.ByteString
dataByteString f = alloca $ \szPtr -> do datPtr <- f szPtr >>= newForeignPtr gdFree . castPtr
                                         liftM (BI.fromForeignPtr datPtr 0 . fromIntegral) (peek szPtr)

--
-- * Text
--

-- | Draw a string using the FreeType 2.x library
drawString :: B.ByteString -- ^ Font name
           -> Double       -- ^ Font point size
           -> Double       -- ^ Angle in counterclockwise radians
           -> Point        -- ^ Origin
           -> B.ByteString -- ^ Text, including HTML entities
           -> Color -> Image -> IO (Point, Point, Point, Point) -- ^ Bounding box of the drawn text
drawString fontName ptSize angle (oriX, oriY) txt color img
    = withImagePtr img $ drawStringImagePtr color fontName ptSize angle (oriX, oriY) txt

-- | Measure a string using the FreeType 2.x library.  This computes
-- the bounding box but does not actually draw the string to any
-- image.
measureString :: B.ByteString -- ^ Font name
              -> Double -- ^ Font point size
              -> Double -- ^ Angle in counterclockwise radians
              -> Point -- ^ Origin
              -> B.ByteString -- ^ Text, including HTML entities
              -> Color -> IO (Point, Point, Point, Point) -- ^ Bounding box of the drawn text
measureString fontName ptSize angle (oriX, oriY) txt color
    = drawStringImagePtr color fontName ptSize angle (oriX, oriY) txt nullPtr

drawStringImagePtr :: Color -> B.ByteString -> Double -> Double -> Point -> B.ByteString -> Ptr GDImage -> IO (Point, Point, Point, Point)
drawStringImagePtr color fontName ptSize angle (oriX, oriY) txt imgPtr
    = allocaArray 8 $
      \bboxPtr -> B.useAsCString fontName $
      \cFontName -> B.useAsCString txt $
      \cTxt -> do res <- gdImageStringFT imgPtr bboxPtr color cFontName (double ptSize) (double angle) (int oriX) (int oriY) cTxt
                  if res == nullPtr
                     then peekArray 8 bboxPtr >>= parseBBox
                     else peekCAString res >>= ioError . userError
    where parseBBox l = case map int l of
                          [llx, lly, lrx, lry, urx, ury, ulx, uly] -> return ((llx, lly), (lrx, lry), (urx, ury), (ulx, uly))
                          _ -> ioError $ userError $ "parseBBox with /= 8 elements: " ++ show l

-- | Draw strings around the top and bottom of a torus
drawStringCircle :: Point -- ^ Center of text path circle
                 -> Double -- ^ Outer radius of text
                 -> Double -- ^ Fraction of radius occupied by text
                 -> Double -- ^ Portion of circle arc filled by text
                 -> B.ByteString -- ^ Font name
                 -> Double -- ^ Font size hint
                 -> B.ByteString -- ^ Text to write on the top of the circle
                 -> B.ByteString -- ^ Text to write on the bottom of the circle
                 -> Color -- ^ Text color
                 -> Image -> IO ()
drawStringCircle (ctrX, ctrY) rad textRad textFill fontName fontSize topTxt bottomTxt color img
    = B.useAsCString fontName $ 
      \cFontName -> B.useAsCString topTxt $
      \cTopTxt -> B.useAsCString bottomTxt $ 
      \cBottomTxt -> withImagePtr img $ 
      \imgPtr -> do res <- gdImageStringFTCircle imgPtr (int ctrX) (int ctrY) (double rad) (double textRad) (double textFill) cFontName (double fontSize) cTopTxt cBottomTxt  color                    
                    unless (res == nullPtr) (peekCAString res >>= ioError . userError)
