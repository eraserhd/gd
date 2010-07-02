module Graphics.GD.ByteString.Lazy (
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
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as BI

import Control.Monad (liftM,unless)
import Foreign
import Foreign.C

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
loadJpegByteString :: L.ByteString -> IO Image
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
loadPngByteString :: L.ByteString -> IO Image
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
loadGifByteString :: L.ByteString -> IO Image
loadGifByteString = onByteStringData loadGifData


loadImageFile :: (Ptr CFILE -> IO (Ptr GDImage)) -> FilePath -> IO Image
loadImageFile f file = 
    do p <- throwIfNull ("Loading image from " ++ file) $ withCFILE file "rb" f
       mkImage p

loadImageData :: (CInt -> Ptr a -> IO (Ptr GDImage)) -> Int -> Ptr a -> IO Image
loadImageData f sz buf =
    do p <- throwIfNull ("Loading image") $ f (fromIntegral sz) buf
       mkImage p

onByteStringData :: (Int -> Ptr a -> IO b) -> L.ByteString -> IO b
onByteStringData f bstr 
    = case BI.toForeignPtr (lazyToStrict bstr) of
        (fptr, start, sz) -> withForeignPtr fptr (\ptr -> f sz (plusPtr ptr start))

lazyToStrict :: L.ByteString -> B.ByteString
lazyToStrict = foldr1 B.append . L.toChunks

--
-- * Saving images
--

-- | Save an image as a JPEG file.
saveJpegFile :: Int -- ^ quality: 0-95, or negative for default quality.
             -> FilePath -> Image -> IO ()
saveJpegFile q = saveImageFile (\p h -> gdImageJpeg p h (fromIntegral q))

-- | Write a JPEG format ByteString of an image.
saveJpegByteString :: Int -> Image -> IO L.ByteString
saveJpegByteString q = saveImageByteString (\p h -> gdImageJpegPtr p h (fromIntegral q))


-- | Save an image as a PNG file.
savePngFile :: FilePath -> Image -> IO ()
savePngFile = saveImageFile gdImagePng

-- | Write a PNG format ByteString of an image.
savePngByteString :: Image -> IO L.ByteString
savePngByteString = saveImageByteString gdImagePngPtr


-- | Save an image as a GIF file.
saveGifFile :: FilePath -> Image -> IO ()
saveGifFile = saveImageFile gdImageGif

-- | Write a GIF format ByteString of an image.
saveGifByteString :: Image -> IO L.ByteString
saveGifByteString = saveImageByteString gdImageGifPtr

saveImageFile :: (Ptr GDImage -> Ptr CFILE -> IO ()) -> FilePath -> Image -> IO ()
saveImageFile f file i = withImagePtr i (\p -> withCFILE file "wb" (f p))

saveImageByteString :: (Ptr GDImage -> Ptr CInt -> IO (Ptr a)) -> Image -> IO (L.ByteString)
saveImageByteString f img = withImagePtr img (\p -> dataByteString (f p))

dataByteString :: (Ptr CInt -> IO (Ptr a)) -> IO L.ByteString
dataByteString f = alloca $ \szPtr -> do datPtr <- f szPtr >>= newForeignPtr gdFree . castPtr
                                         liftM (L.fromChunks . return . BI.fromForeignPtr datPtr 0 . fromIntegral) (peek szPtr)

--
-- * Text
--

-- | Draw a ByteString using the FreeType 2.x library
drawString :: L.ByteString -- ^ Font name
             -> Double       -- ^ Font point size
             -> Double       -- ^ Angle in counterclockwise radians
             -> Point        -- ^ Origin
             -> L.ByteString -- ^ Text, including HTML entities
             -> Color -> Image -> IO (Point, Point, Point, Point) -- ^ Bounding box of the drawn text
drawString fontName ptSize angle (oriX, oriY) txt color img
    = withImagePtr img $ drawStringImagePtr color fontName ptSize angle (oriX, oriY) txt

-- | Measure a string using the FreeType 2.x library.  This computes
-- the bounding box but does not actually draw the string to any
-- image.
measureString :: L.ByteString -- ^ Font name
              -> Double -- ^ Font point size
              -> Double -- ^ Angle in counterclockwise radians
              -> Point -- ^ Origin
              -> L.ByteString -- ^ Text, including HTML entities
              -> Color -> IO (Point, Point, Point, Point) -- ^ Bounding box of the drawn text
measureString fontName ptSize angle (oriX, oriY) txt color
    = drawStringImagePtr color fontName ptSize angle (oriX, oriY) txt nullPtr

drawStringImagePtr :: Color -> L.ByteString -> Double -> Double -> Point -> L.ByteString -> Ptr GDImage -> IO (Point, Point, Point, Point)
drawStringImagePtr color fontName ptSize angle (oriX, oriY) txt imgPtr
    = allocaArray 8 $
      \bboxPtr -> toCStr fontName $
      \cFontName -> toCStr txt $
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
                 -> L.ByteString -- ^ Font name
                 -> Double -- ^ Font size hint
                 -> L.ByteString -- ^ Text to write on the top of the circle
                 -> L.ByteString -- ^ Text to write on the bottom of the circle
                 -> Color -- ^ Text color
                 -> Image -> IO ()
drawStringCircle (ctrX, ctrY) rad textRad textFill fontName fontSize topTxt bottomTxt color img
    = toCStr fontName $ 
      \cFontName -> toCStr topTxt $
      \cTopTxt -> toCStr bottomTxt $ 
      \cBottomTxt -> withImagePtr img $ 
      \imgPtr -> do res <- gdImageStringFTCircle imgPtr (int ctrX) (int ctrY) (double rad) (double textRad) (double textFill) cFontName (double fontSize) cTopTxt cBottomTxt  color                    
                    unless (res == nullPtr) (peekCAString res >>= ioError . userError)

toCStr :: L.ByteString -> (CString -> IO a) -> IO a
toCStr = B.useAsCString . lazyToStrict
