TUI_SOURCES = src/MyRandom.scala src/ZigZagUtils.scala src/ZigZag.scala
GUI_SOURCES = src/MyRandom.scala src/ZigZagUtils.scala src/ZigZagGUI.scala
TUI_JAR     = out/zigzag.jar
GUI_JAR     = out/zigzag-gui.jar

build:
	mkdir -p out
	scala --power package $(TUI_SOURCES) --standalone --main-class ZigZag -o $(TUI_JAR) -f

run: build
	exec java -jar $(TUI_JAR)

build-gui:
	mkdir -p out
	scala --power package $(GUI_SOURCES) --standalone --main-class ZigZagGUI -o $(GUI_JAR) -f

gui: build-gui
	exec java -jar $(GUI_JAR)

.PHONY: build run build-gui gui
