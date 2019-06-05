src = invaders.asm

.PHONY: all
all: invaders.img invaders.com invd8088.com

invaders.img: $(src)
	nasm -f bin -o $@ $(src)

invaders.com: $(src)
	nasm -f bin -o $@ -Dcom_file=1 $(src)

invd8088.com: $(src)
	nasm -f bin -o $@ -Dcom_file=1 -Dpure8088=1 $(src)

.PHONY: clean
clean:
	$(RM) invaders.img invaders.com invd8088.com
