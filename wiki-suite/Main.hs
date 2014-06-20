-- TO test: ghci wiki-suite/Draw_Canvas.hs -idist/build/autogen/:.:wiki-suite
import qualified Arc
import qualified Bezier_Curve
import qualified Bounce
import qualified Circle
import qualified Clipping_Region
import qualified Color_Fill
import qualified Color_Square
import qualified Custom_Shape
import qualified Draw_Canvas
import qualified Draw_Device
import qualified Draw_Image
import qualified Favicon
import qualified Font_Size_and_Style
import qualified Get_Image_Data_URL
import qualified Global_Alpha
import qualified Global_Composite_Operations
import qualified Grayscale
import qualified Image_Crop
import qualified Image_Loader
import qualified Image_Size
import qualified Is_Point_In_Path
import qualified Key_Read
import qualified Line
import qualified Line_Cap
import qualified Line_Color
import qualified Line_Join
import qualified Line_Width
import qualified Linear_Gradient
import qualified Load_Image_Data_URL
import qualified Miter_Limit
import qualified Path
import qualified Pattern
import qualified Quadratic_Curve
import qualified Radial_Gradient
import qualified Rectangle
import qualified Red_Line
import qualified Rotating_Square
import qualified Rounded_Corners
import qualified Semicircle
import qualified Shadow
import qualified Square
import qualified Text_Align
import qualified Text_Baseline
import qualified Text_Color
import qualified Text_Metrics
import qualified Text_Stroke
import qualified Text_Wrap
import qualified Tic_Tac_Toe
import qualified Translate_Transform
import qualified Scale_Transform
import qualified Rotate_Transform
import qualified Custom_Transform

import System.Environment

main = do 
     args <- getArgs 
     main2 args

main2 ["Arc"] = Arc.main
main2 ["Bezier_Curve"] = Bezier_Curve.main
main2 ["Bounce"] = Bounce.main
main2 ["Circle"] = Circle.main
main2 ["Clipping_Region"] = Clipping_Region.main
main2 ["Color_Fill"] = Color_Fill.main
main2 ["Color_Square"] = Color_Square.main
main2 ["Custom_Shape"] = Custom_Shape.main
main2 ["Draw_Canvas"] = Draw_Canvas.main
main2 ["Draw_Device"] = Draw_Device.main
main2 ["Draw_Image"] = Draw_Image.main
main2 ["Favicon"] = Favicon.main
main2 ["Font_Size_and_Style"] = Font_Size_and_Style.main
main2 ["Get_Image_Data_URL"] = Get_Image_Data_URL.main
main2 ["Global_Alpha"] = Global_Alpha.main
main2 ["Global_Composite_Operations"] = Global_Composite_Operations.main
main2 ["Grayscale"] = Grayscale.main
main2 ["Image_Crop"] = Image_Crop.main
main2 ["Image_Loader"] = Image_Loader.main
main2 ["Miter_Limit"] = Miter_Limit.main
main2 ["Image_Size"] = Image_Size.main
main2 ["Is_Point_In_Path"] = Is_Point_In_Path.main
main2 ["Key_Read"] = Key_Read.main
main2 ["Line"] = Line.main
main2 ["Line_Cap"] = Line_Cap.main
main2 ["Line_Color"] = Line_Color.main
main2 ["Line_Join"] = Line_Join.main
main2 ["Line_Width"] = Line_Width.main
main2 ["Linear_Gradient"] = Linear_Gradient.main
main2 ["Load_Image_Data_URL"] = Load_Image_Data_URL.main
main2 ["Path"] = Path.main
main2 ["Pattern"] = Pattern.main
main2 ["Quadratic_Curve"] = Quadratic_Curve.main
main2 ["Radial_Gradient"] = Radial_Gradient.main
main2 ["Rectangle"] = Rectangle.main
main2 ["Red_Line"] = Red_Line.main
main2 ["Rotating_Square"] = Rotating_Square.main
main2 ["Rounded_Corners"] = Rounded_Corners.main
main2 ["Semicircle"] = Semicircle.main
main2 ["Shadow"] = Shadow.main
main2 ["Square"] = Square.main
main2 ["Text_Align"] = Text_Align.main
main2 ["Text_Baseline"] = Text_Baseline.main
main2 ["Text_Color"] = Text_Color.main
main2 ["Text_Metrics"] = Text_Metrics.main
main2 ["Text_Stroke"] = Text_Stroke.main
main2 ["Text_Wrap"] = Text_Wrap.main
main2 ["Tic_Tac_Toe"] = Tic_Tac_Toe.main
main2 ["Translate_Transform"] = Translate_Transform.main
main2 ["Scale_Transform"] = Scale_Transform.main
main2 ["Rotate_Transform"] = Rotate_Transform.main
main2 ["Custom_Transform"] = Custom_Transform.main


