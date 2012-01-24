import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= clear [ ColorBuffer ]
  mainLoop
