import Graphics.UI.Gtk

main = do
	initGUI
	w <- windowNew
	l <- labelNew (Just "a")
	let f = do
		labelGetText l >>= labelSetText l . (++ "a") 
		labelGetAngle l >>= labelSetAngle l . (+ 3)
		return True
	timeoutAdd f 100
	containerAdd w l
	widgetShowAll w
	onDestroy w mainQuit
	mainGUI


