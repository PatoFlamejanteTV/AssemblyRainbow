# AssemblyRainbow
Draws a beautiful moving rainbow in x64 bios (Long Mode).
![Screenshot](https://raw.githubusercontent.com/Paul-Andre/AssemblyRainbow/master/Screenshot_2016-09-12_23-02-41.png)

Assemble it:
```bash
nasm rainbow.asm
```

Run it in qemu emulator:
```bash
qemu-system-x86_64 -drive format=raw,file=rainbow
```

Alternatively, you can put this in the first sector of a usb-stick or floppy-drive and boot a computer with it.
