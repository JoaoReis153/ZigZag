TUI_SOURCES = src/MyRandom.scala src/ZigZagUtils.scala src/ZigZag.scala
TUI_JAR     = out/zigzag.jar

build:
	mkdir -p out
	scala --power package $(TUI_SOURCES) --standalone --main-class ZigZag -o $(TUI_JAR) -f

run: build
	exec java -jar $(TUI_JAR)

.PHONY: build run
