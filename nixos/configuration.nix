# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
nixpkgs.overlays = [
    (import ./overlays.nix)
    (import /home/vamshi/.config/taffybar/taffybar/environment.nix)
  ];

  # Allow all the things
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.android_sdk.accept_license = true;

  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
  ];

  # bluetooth
   hardware.bluetooth = {
       enable = true;
           extraConfig = ''
	         [General]
		Enable=Source,Sink,Media,Socket
		'';
		};
  services.blueman.enable = true;
  sound.enable = true;
  hardware.pulseaudio = {
	       enable = true;
               extraModules = [ pkgs.pulseaudio-modules-bt ];
	       package = pkgs.pulseaudioFull;
               support32Bit = true; # Steam
	       extraConfig = ''
               load-module module-bluetooth-policy auto_switch=2
	         '';
   };
 # hardware.bluetooth.enable = true;

 # hardware.pulseaudio = {
 #     enable = true;
 #     extraConfig = ''
 #      [General]
 #      Enable=Source,Sink,Media,Socket
 #     '';
          # NixOS allows either a lightweight build (default) or full build of PulseAudio to be installed.
	  # Only the full build has Bluetooth support, so it must be selected here.
     #package = pkgs.pulseaudioFull;
 #          };
  
  programs.light.enable = true;
#  services.redshift = {
#      enable = true;
#      provider = "geoclue2";
#          brightness = {
#	        # Note the string values below.
#	       day = "0.5";
#	       night = "0.5";
#	     };
#	     temperature = {
#	           day = 3400;
#	           night = 3400;
#	        };
#	   };
#services.redshift = {
#      enable = true;
#      latitude = "17.492811";
#      longitude = "78.281268";
#      brightness = {
#        # Day and night mixed up, lol
#        day = "0.4";
#        night = "0.4";
#      };
      #tray = false;
#      provider = "manual";
#      temperature = {
        # Day and night mixed up, lol
#        night = 3401;
#        day = 3401;
#      };
#    extraOptions = ["-v"];
#    };
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "cosmos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  
   services.xserver.libinput.naturalScrolling = true;
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp1s0.useDHCP = true;
  networking.interfaces.wlp2s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_IN.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
    services.udev.extraRules = ''
         ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", MODE="0666", RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
    '';
  services.upower.enable = true;
  fonts = {
  fonts = with pkgs; [
      dejavu_fonts
      emojione
      fira-code
      fira-code-symbols
      powerline-fonts
      font-awesome-ttf
      noto-fonts-emoji
      roboto
      #sans
      #lucida-MAC
      #lucida-grande
      source-code-pro
      source-sans-pro
      source-serif-pro
      twemoji-color-font
    ];
    fontconfig = {
      defaultFonts = {
        monospace = [ "Fira Code Mono" ];
        sansSerif = [ "Roboto" ];
        serif     = [ "Source Serif Pro" ];
      };
    };
  };
  # Set your time zone.
  time.timeZone = "Asia/Kolkata";

 # location.latitude  = 17.492829;
 # location.longitude = 78.281284;

   xdg.menus.enable = true;


  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    automake
    xorg.xkill
    neovim 
    firefox
    feh
    git
    htop
    yadm
    autorandr
    betterlockscreen
    dzen2
    pulsemixer
    notify-desktop
    okular
    neofetch
    pfetch
    konsole
    kdenlive
    redshift
    tree
    brave
    picom
    nitrogen
    #xorg.xrandr
    unzip
    emacs
    scrot
    #redshift-plasma-applet
    dmenu
    xz
    # Haskell
    #cabal-install
    #cabal2nix
   #ghc
    #ghcid
    #X11
    #xorg.libX11
    #xorg.libXrandr
    #xorg.libXinerama
    # stack
    #haskellPackages.hpack
    #haskellPackages.hasktags
    #haskellPackages.hoogle
    #xmonad
    xmonad-with-packages
    #spotify
    vlc
    #zoom-us
    # xorg
    wmctrl
    xclip
    xdotool
    xorg.xev
    xorg.xdpyinfo
    xorg.xkbcomp
    xorg.xwininfo
    xsettingsd
    xorg.xbacklight
    # Appearance
    gnome-breeze
    gnome3.adwaita-icon-theme
    hicolor-icon-theme
    materia-theme
    numix-icon-theme-circle
    papirus-icon-theme
    plasma5.breeze-gtk
    plasma5.breeze-qt5
    rofi
    tmux

    pcmanfm
    binutils-unwrapped
    # Nix
    nix-prefetch-git
    cachix
    termonad-with-packages
    # Haskell
    cabal-install
    cabal2nix
    ghc
    #stack2nix
    # stack
    haskellPackages.hpack
    haskellPackages.hasktags
    haskellPackages.hoogle
    #haskellPackages.xmonad-wallpaper
    #haskellPackages.split
    # (all-hies.selection { selector = p: { inherit (p) ghc864 ghc865; }; })
    #xmobar
    #xmobar
    zlib
    gnome3.zenity
    lxqt.lxqt-powermanagement
    # xmonad
    haskellPackages.status-notifier-item
    haskellPackages.xmonad
    haskellPackages.dbus-hslogger
    # dhall
    haskellPackages.dhall
    haskellPackages.dhall-json
    # Haskell Desktop
    (import /home/vamshi/.config/taffybar/default.nix)
    (import /home/vamshi/.config/xmonad/default.nix)
    (import /home/vamshi/repo/gtk-sni-tray/default.nix)
    (import /home/vamshi/repo/status-notifier-item/default.nix)

    #jdk
    jdk
    go
    ];
 xdg.portal.enable = true;
 services.flatpak.enable = true;
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   pinentryFlavor = "gnome3";
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  #sound.enable = true;
  #hardware.pulseaudio.enable = true;
  #networking.networkmanager.enable = true;

  # Enable the X11 windowing system.
  #services.xserver.enable = true;
  #services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  #services.xserver.displayManager.defaultSession = "none+xmonad";
  #services.xserver.desktopManager.xfce.enable = true;
  #services.xserver.displayManager.gdm.enable = true;
  #services.xserver.desktopManager.gnome3.enable = true;
#services.xserver.displayManager.defaultSession = "none+xmonad";
    	services.xserver = {
        	enable = true;
        	displayManager = {
			defaultSession = "none+xmonad";
			lightdm.greeters.mini = {
            		enable = true;
            		user = "vamshi";
          	  	extraConfig = ''
                		[greeter]
               			show-password-label = false
        	        	[greeter-theme]
        	        	background-image = "/home/vamshi/Pictures/greeter.jpg"
        	    		'';
        		};
		};
        	windowManager = {
		 session = [
        {
          name = "xmonad";
          start = ''
            /usr/bin/env imalison-xmonad &
            waitPID=$!
          '';
        }
	];
	};
	};

        	#    #default = "xmonad";
        	#    xmonad.enable = true;
        	#};
#	    };
  #services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager = {
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
      #default = "xmonad";
    };
   # services.xserver = {
   # exportConfiguration = true;
   # enable = true;
   # layout = "us";
   # desktopManager = {
   #   plasma5.enable = true;
   # };
   # windowManager = {
   #   session = [
   #     {
   #       name = "xmonad";
   #       start = ''
   #         /usr/bin/env imalison-xmonad &
  #          waitPID=$!
  #        '';
  #      }
  #   ];
  #   };
  #    displayManager = {
  #    sddm = {
  #       enable = true;
  #    };
  #   sessionCommands = ''
  #     systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
  #    '';
  #  };
  # };
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.vamshi = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };
  #users.users.vamshi.extraGroups = [ "networkmanager" ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}

