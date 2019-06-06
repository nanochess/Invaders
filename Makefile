# Makefile contributed by jtsiomb

src = invaders.asm

.PHONY: all
all: invaders.img invaders.com

invaders.img: $(src)
	nasm -f bin -o $@ $(src)

invaders.com: $(src)
	nasm -f bin -o $@ -Dcom_file=1 $(src)

.PHONY: clean
clean:
	$(RM) invaders.img invaders.com

.PHONY: rundosbox
rundosbox: invaders.com
	dosbox $<

.PHONY: runqemu
runqemu: invaders.img
	qemu-system-i386 -fda invaders.img
