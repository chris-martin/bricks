{ code, file-path, file-string html-tags, html, scss, markdown }:

let
  inherit (html-tags) ul h2 h3;

  p  = x: html-tags.p  (markdown x);
  li = x: html-tags.li (markdown x);

  bash = code { language = "bash"; };
  nix  = code { language = "nix"; };

  image = style: x: html ''
    <div style="text-align: center; ${style}">
      <img src="${file-path x}" style="max-width: 100%;"/>
    </div>
  '';

  kozar-tweet = "https://twitter.com/iElectric/status/643359273012043776";
  mlabel      = "http://askubuntu.com/a/103695/333663";
  bluishcoder = "http://bluishcoder.co.nz/2014/05/14/installing-nixos-with-encrypted-root-on-thinkpad-w540.html";

in {
  title = "Installing NixOS";
  date  = "2015 Sept 27";
  slug  = "installing-nixos";

  css = scss ./installing-nixos.scss;

  thumbnail = file-path ./passphrase-prompt.jpg;

  abstract = ''
    I just installed NixOS on my laptop. This is a summary of my experience.
  '';

  body = [

    (p ''
      I just installed NixOS on my laptop. This is a summary of my experience.
    '')

    (p ''
      I hope you find it useful if you’re new to NixOS or setting up full disk
      encryption. Feel free to tweet questions at me and I’ll help if I can,
      though you’ll find more knowledgable folks in the `#nixos` channel on
      Freenode.
    '')

    (p ''
      I keep my NixOS configuration in
      [a git repository](https://github.com/chris-martin/nixos-config).
      I recommend learning from other people’s configuration files and sharing
      your own.
    '')

    (p ''
      Much of this follows Bluish Coder’s
      [Installing NixOS on a ThinkPad W540 with encrypted root](${bluishcoder})
      and
      [Encrypted Root on NixOS](https://nixos.org/wiki/Encrypted_Root_on_NixOS)
      on the NixOS wiki.
    '')

    ###################################################################

    (h2 "My requirements, constraints, and choices")

    (ul [
      (li "The hardware: Samsung Series 9 NP900X4C-A06US")
      (li "Replacing the entire disk’s content, preserving nothing.")
      (li "Must have encrypted root and swap.")
      (li "Laptop has no CD drive, so must boot from USB.")
      (li "Laptop has no ethernet port, so must use wifi (more specifically, WPA) during installation.")
      (li "UEFI boot.")
    ])

    ###################################################################

    (h2 "Preview")

    (p "Here’s what it’s going to look like once NixOS is installed.")

    ###################################################################

    (h3 "The boot loader")

    (p ''
      Here’s what my boot loader looks like after I’ve been using this system
      for a couple weeks and have rebuilt the configuration 114 times.
    '')

    (image "" ./boot-loader.jpg)

    ###################################################################

    (h3 "Unlocking the encrypted disk")

    (p ''
      In the first stage of the boot process, NixOS stops to let you enter
      the passphrase before proceeding.
    '')

    (image "" ./passphrase-prompt.jpg)

    ###################################################################

    (h3 "The default login prompt")

    (image "" ./login-prompt.jpg)

    ###################################################################

    (h2 "Creating NixOS USB installer")

    (p ''
      Download NixOS
      [15.09.336.83cc494](https://nixos.org/releases/nixos/15.09/nixos-15.09.336.83cc494/)
      x86_64 minimal install ISO.
    '')

    (html ''
      <p style="font-style: italic;">
        (I first tried the latest stable release, 14.12, and had an error
        installing Grub. I switched to this particular build of NixOS 15.09
        on <a href="${kozar-tweet}">Domen Kožar’s recommendation</a>.)
      </p>
    '')

    (p "Load it onto a flash drive using unetbootin.")

    (html ''
      <p style="font-style: italic;">
        (I don’t know if there’s a better way to do this. Creating USB install
        media is still a huge mystery to me; I don’t get why it’s so complicated
        or why there’s seemingly no authoritative method.)
      </p>
    '')

    (p "The volume label must be `NIXOS_ISO`:")

    (bash ''
      sudo mlabel -i <device> -s ::"NIXOS_ISO  "
    '')

    (p ''
      Note the spaces that pad the label name to
      [exactly 11 characters](${mlabel}).
    '')

    (p ''
      Boot the NixOS live "CD". It contains the utilities that you’ll need to
      set up the disk, and of course the NixOS installer.
    '')

    ###################################################################

    (h2 "Disk layout")

    (p ''
      Here’s a somewhat high-level depiction of the disk we’re about to set up.
    '')

    (image "margin-top: 3em;" ./disk.png)

    (p "There are three partitions:")

    (ul [
      (li ''
        `/dev/sda1` is the BIOS boot partition. Don’t ask me exactly what this
        is for, all I know is it has to be there.
      '')
      (li ''
        `/dev/sda2` will be mounted at `/boot`. This is unencrypted,
        because the boot process will need it before we unlock the encrypted
        volume.
      '')
      (li "`/dev/sda3` is the encrypted partition.")
    ])

    (p ''
      The encrypted partition contains an LVM volume group called `vg`, which
      contains two logical volumes:
    '')

    (ul [
      (li "`/dev/vg/swap` will be used as swap space.")
      (li "`/dev/vg/root` will be mounted at the root of the filesystem, `/`.")
    ])

    ###################################################################

    (h3 "Create partitions")

    (p "Run `fdisk -l` to show disks. Mine is `/dev/sda`.")

    (p "Use `gdisk` to create the partitions.")

    (bash "gdisk /dev/sda")

    (p ''
      I won’t teach you how to use `gdisk` here, but create a partition table
      that looks like this:
    '')

    (code {} ''
      Number  Size        Code  Name
        1     1000.0 KiB  EF02  BIOS boot partition
        2     500.0 MiB   EF00  EFI System Partition
        3     <the rest>  8E00  Linux LVM
    '')

    ###################################################################

    (h3 "Set up LUKS")

    (p ''
      Initialize the encrypted partition. This will prompt you to create a
      passphrase (which can be changed later, so don’t fret if you haven’t
      generated a good one yet).
    '')

    (bash "cryptsetup luksFormat /dev/sda3")

    (p "Then open it:")

    (bash "cryptsetup luksOpen /dev/sda3 enc-pv")

    (p ''
      I have no idea what `enc-pv` means here, I’m just following instructions.
    '')

    ###################################################################

    (h3 "Create LVM group and volumes")

    (p "This allocates 10G for swap, and the rest for the root volume.")

    (bash ''
      pvcreate /dev/mapper/enc-pv
      vgcreate vg /dev/mapper/enc-pv
      lvcreate -n swap vg -L 10G
      lvcreate -n root vg -l 100%FREE
    '')

    ###################################################################

    (h3 "Format partitions")

    (bash ''
      mkfs.vfat -n BOOT /dev/sda2
      mkfs.ext4 -L root /dev/vg/root
      mkswap -L swap /dev/vg/swap
    '')

    ###################################################################

    (h2 "Installation")

    ###################################################################

    (h3 "Mount")

    (p ''
      Now mount all the disks and volumes you just created. But not where we
      said we were going to mount them.
    '')

    (p ''
      The NixOS installer treats `/mnt` as the filesystem root for the
      installation. So instead of mounting to `/` and `/boot` as we discussed
      above, for now we’re going to mount them to `/mnt` and `/mnt/boot`
      instead:
    '')

    (bash ''
      mount /dev/vg/root /mnt
      mkdir /mnt/boot
      mount /dev/sda2 /mnt/boot
    '')

    (p "Also activate the swap space:")

    (bash "swapon /dev/vg/swap")

    ###################################################################

    (h3 "Configuration")

    (p "Run this to generate config files:")

    (bash "nixos-generate-config --root /mnt")

    (p "This creates two files in `/mnt/etc/nixos`:")

    (ul [
      (li ''
        `configuration.nix`, a default config file.
        (You’ll be making changes to this a lot).
      '')
      (li ''
        `hardware-configuration.nix`, the results of a hardware scan.
        (You don’t edit this.)
      '')
    ])

    (p "Add this stuff to the `configuration.nix`:")

    (nix ''
      boot.initrd.luks.devices = [
        {
          name = "root";
          device = "/dev/sda3";
          preLVM = true;
        }
      ];

      boot.loader.grub.device = "/dev/sda";

      networking.wireless.enable = true;
    '')

    (p ''
      Hopefully its meaning is mostly self-explanatory, as NixOS configuration
      tends to be.
    '')

    ###################################################################

    (h3 "Wifi")

    (p ''
      Disable the `wpa_supplicant` service that’s running, and run it yourself
      specifying the WPA credentials.
    '')

    (bash ''
      systemctl stop wpa_supplicant.service
      wpa_supplicant -B -i interface -c <(wpa_passphrase 'SSID' 'key')
    '')

    (p ''
      I’m not sure why, exactly, but that’s what
      [the wiki](https://nixos.org/wiki/Wpa_supplicant)
      says to do.
    '')

    ###################################################################

    (h3 "If you reboot during this process")

    (p ''
      After you’ve created the partitions and LVM volumes, here’s a recap of
      everything you need to set up when you boot from the install media again.
    '')

    (bash ''
      cryptsetup luksOpen /dev/sda3 enc-pv
      lvchange -a y /dev/vg/swap
      lvchange -a y /dev/vg/root
      mount /dev/vg/root /mnt
      mount /dev/sda2 /mnt/boot
      swapon /dev/vg/swap
      systemctl stop wpa_supplicant.service
      wpa_supplicant -B -i interface -c <(wpa_passphrase 'SSID' 'key')
    '')

    ###################################################################

    (h3 "Install")

    (bash "nixos-install")

    (p ''
      If that succeeds, then reboot and proceed to boot into your new NixOS
      install.
    '')

    (bash "reboot")

    ###################################################################

    (h2 "Your first NixOS boot")

    (p ''
      At this point, there is now a ton of fun stuff to learn that is outside
      the scope of this walkthrough. I’m just going to cover the a little bit
      of essential configuration to get you started in a graphical environment.
    '')

    ###################################################################

    (h3 "Connect to wifi again")

    (p ''
      We’ll get a network manager set up soon, but for now run these commands
      again to get back on wifi.
    '')

    (bash ''
      systemctl stop wpa_supplicant.service
      wpa_supplicant -B -i interface -c <(wpa_passphrase 'SSID' 'key')
    '')

    ###################################################################

    (h3 "Add the 15.09 package channel")

    (p "In a root shell:")

    (bash ''
      nix-channel --add http://nixos.org/channels/nixos-15.09
      nix-channel --update
    '')

    ###################################################################

    (h3 "Editing the config file")

    (p "You’ll be editing `/etc/nixos/configuration.nix` a lot.")

    (p "When you do, run `nixos-rebuild switch` to apply the changes.")

    ###################################################################

    (h3 "Create a user")

    (p "Here’s my initial user config:")

    (nix ''
      users.extraUsers.chris = {
        name = "chris";
        group = "users";
        extraGroups = [
          "wheel" "disk" "audio" "video"
          "networkmanager" "systemd-journal"
        ];
        createHome = true;
        uid = 1000;
        home = "/home/chris";
        shell = "/run/current-system/sw/bin/bash";
      };
    '')

    (p ''
      Its password obviously doesn’t go in the config file, so
      you have to run this command to set it:
    '')

    (bash "passwd chris")

    ###################################################################

    (h3 "Enable graphical environment")

    (p "Add this config to enable KDE and the touchpad:")

    (nix ''
      services.xserver = {
        enable = true;
        desktopManager.kde4.enable = true;
        synaptics.enable = true;
      };
    '')

    (html ''
      <p style="font-style: italic;">
        (I haven’t used KDE in a long time, but I decided on a whim to try it
        again—and I am delightfully surprised by how it has improved.)
      </p>
    '')

    ###################################################################

    (h3 "Enable network management")

    (p ''
      Now you’ll want to turn on the network manager which can manage your
      WPA keys so you don’t have to keep manually messing with `wpa_supplicant`.
    '')

    (p "Replace")

    (nix "networking.wireless.enable = true;")

    (p "with")

    (nix "networking.networkmanager.enable = true;")

    (p ''
      Also add `kde4.networkmanagement` to the package list to get a GUI for it.
    '')

    ###################################################################

    (h2 "Reference documentation")

    (p "If you add this config:")

    (nix "services.nixosManual.showManual = true;")

    (p ''
      Then ctrl+alt+F8 gives you the manual in a terminal, which may be nice
      to keep around in case of emergency.
    '')

    (p "`nixos-help` opens the manual in a browser window.")

    (p "`nixos-option` is a good reference for the configuration options.")

    (p ''
      To search for packages, use `nix-env -qaP | grep ...`.
      The left column shows attribute paths (which is what you need for
      `systemPackages` in the config), and the right column shows package names.
    '')

    br br

    (html (file-string ./tweet.html))

  ];
}
