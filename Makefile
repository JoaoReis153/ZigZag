SOURCES = src/MyRandom.scala src/ZigZagUtils.scala src/ZigZag.scala
OUT = out

build:
	mkdir -p $(OUT)
	scalac $(SOURCES) -d $(OUT)

run:
	scala run $(SOURCES)

.PHONY: build run
