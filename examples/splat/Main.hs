import Graphics.Blank

test n = do
                (width,height) <- size
                save()
                translate (width / 2,height / 2)
                rotate (pi * n)
                beginPath()
                moveTo(-200,-200)
                lineTo(-200,200)
                lineTo(200,200)
                lineTo(200,-200)
                closePath()
                lineWidth 25
                strokeStyle "orange"
                stroke()
                restore()

splat = splatCanvas 3000

main = do
     splat (>> test 0)
     splat (>> test 0.1)
     splat (>> test 0.2)
     splat (>> test 0.3)
     print "only works inside ghci"

