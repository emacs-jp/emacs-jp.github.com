---
layout: page
author: conao3
title: "Emacsのインストール"
date: 2020-08-25
last_modified: 2020-08-27
---
{% include JB/setup %}

## はじめに

この記事は各OSにおけるEmacsのインストール方法をまとめるものです。

まず、簡便なインストール方法としてパッケージマネージャを用いたインストール方法を紹介します。
初心者の方はまずこちらの方法でインストールしてみて下さい。

パッケージマネージャで提供されていない新しいEmacsを使用したい場合や、コア部分にパッチを当てたい場合などにおいては自分でmakeする必要があります。

## パッケージマネージャ
### Windows

WindowsについてはMSYS2でインストールする方法が推奨されています。

```
pacman -S mingw-w64-x86_64-emacs
```

### macOS

macOSについてはbrewでインストールできます。

```
brew cask install emacs
```

### Linux

Linux各ディストリビューションについては、それぞれのパッケージマネージャでインストールできます。
各ディストリビューションでインストールできるバージョンについては「[repology.org - Versions for emacs](https://repology.org/project/emacs/versions)」で一覧できます。
```
apt install install emacs
```

## make

### Windows

Windowsにおけるmakeについては外部記事が詳しいです。([Qiita - Windwos版 Emacs 27.1 / HEAD コンパイル手順メモ](https://qiita.com/kawabata@github/items/76239e8f39eb8ed4a79d))

### macOS

macOSにおけるmakeについては外部記事が詳しいです。([Qiita - Emacs-25.3/26.3(Mojave, Catalina)/27.1/28.x にインラインパッチをあてて使う（macOS）](https://qiita.com/takaxp/items/e07bb286d80fa9dd8e05))

### Linux
Dockerfileで示します。ローカルで立てて、開発環境とする意図のものです。
参照用であり、このまま利用することは想定していません。

システムのEmacsと競合しないように `$HOME/.local` 以下にインストールする手順になっています。
`build-deps` やそれに類するコマンドを使用した場合、具体的にインストールされたパッケージをコマントで示しました。

なお、以下のDockerfileは以下のコマンドを入力することにより、GUI版のEmacsを起動できます。
```
docker image build -t emacs-arch -f Dockerfile-arch .
docker run -it --rm -v /tmp/.X11-unix:/tmp/.X11-unix -v $HOME:/home/user/host -e DISPLAY=$DISPLAY --user 1000 emacs-arch emacs
```

- Arch
  ```dockerfile
  FROM archlinux:20200705
  
  MAINTAINER conao3
  
  RUN echo '# mirrorlist' > /etc/pacman.d/mirrorlist && \
      echo 'Server = http://ftp.tsukuba.wide.ad.jp/Linux/archlinux/$repo/os/$arch' >> /etc/pacman.d/mirrorlist && \
      echo 'Server = http://ftp.jaist.ac.jp/pub/Linux/ArchLinux/$repo/os/$arch' >> /etc/pacman.d/mirrorlist && \
      ln -sf /usr/share/zoneinfo/Asia/Tokyo /etc/localtime && \
      sed -ie 's/#Color/Color/' /etc/pacman.conf && \
      sed -ie 's/#VerbosePkgLists/VerbosePkgLists/' /etc/pacman.conf && \
      pacman-key --init && \
      pacman-key --populate archlinux && \
      pacman -Syu --noconfirm
  
  ##############################
  
  ARG USER=user
  ARG UID=1000
  ARG GID=1000
  ARG HOME=/home/${USER}
  ARG SHELL=/usr/sbin/bash
  
  RUN pacman -S sudo --noconfirm && \
      groupadd -g ${GID} ${USER} && \
      useradd -u ${UID} -g ${GID} -m -s ${SHELL} ${USER} && \
      passwd -d ${USER} && \
      echo "${USER} ALL=(ALL) ALL" >> /etc/sudoers && \
      sed -ie 's/^Defaults\tenv_reset/# Defaults\tenv_reset/' /etc/sudoers && \
      sed -ie 's/^Defaults\tsecure_path/# Defaults\tsecure_path/' /etc/sudoers
  
  ##############################
  
  USER ${USER}
  WORKDIR ${HOME}
  
  ENV PATH=/home/user/.local/bin:$PATH
  ENV EDITOR=emacs
  ARG VERSION=27.1
  
  RUN mkdir -p ${HOME}/.local/work && \
      cd ${HOME}/.local/work && \
      curl -LO https://ftp.gnu.org/gnu/emacs/emacs-$VERSION.tar.gz && \
      tar xf emacs-$VERSION.tar.gz
  
  RUN sudo pacman -S base-devel --noconfirm
  RUN sudo pacman -S man git --noconfirm
  RUN sudo pacman -S gtk3 --noconfirm
  RUN sudo pacman -S libxpm giflib --noconfirm
  RUN sudo pacman -S gpm libotf m17n-lib jansson --noconfirm
  
  RUN cd ${HOME}/.local/work/emacs-$VERSION && \
      ./autogen.sh && \
      ./configure --prefix=$HOME/.local && \
      make -j && make install
  
  CMD ["bash"]
  
  # Configured for 'x86_64-pc-linux-gnu'.
  #   Where should the build process find the source code?    .
  #   What compiler should emacs be built with?               gcc -g3 -O2
  #   Should Emacs use the GNU version of malloc?             no
  #     (The GNU allocators don't work with this system configuration.)
  #   Should Emacs use a relocating allocator for buffers?    no
  #   Should Emacs use mmap(2) for buffer allocation?         no
  #   What window system should Emacs use?                    x11
  #   What toolkit should Emacs use?                          GTK3
  #   Where do we find X Windows header files?                Standard dirs
  #   Where do we find X Windows libraries?                   Standard dirs
  #   Does Emacs use -lXaw3d?                                 no
  #   Does Emacs use -lXpm?                                   yes
  #   Does Emacs use -ljpeg?                                  yes
  #   Does Emacs use -ltiff?                                  yes
  #   Does Emacs use a gif library?                           yes -lgif
  #   Does Emacs use a png library?                           yes -lpng16 -lz 
  #   Does Emacs use -lrsvg-2?                                yes
  #   Does Emacs use cairo?                                   no
  #   Does Emacs use -llcms2?                                 yes
  #   Does Emacs use imagemagick?                             no
  #   Does Emacs support sound?                               yes
  #   Does Emacs use -lgpm?                                   yes
  #   Does Emacs use -ldbus?                                  yes
  #   Does Emacs use -lgconf?                                 no
  #   Does Emacs use GSettings?                               yes
  #   Does Emacs use a file notification library?             yes -lglibc (inotify)
  #   Does Emacs use access control lists?                    yes -lacl
  #   Does Emacs use -lselinux?                               no
  #   Does Emacs use -lgnutls?                                yes
  #   Does Emacs use -lxml2?                                  yes
  #   Does Emacs use -lfreetype?                              yes
  #   Does Emacs use HarfBuzz?                                yes
  #   Does Emacs use -lm17n-flt?                              yes
  #   Does Emacs use -lotf?                                   yes
  #   Does Emacs use -lxft?                                   yes
  #   Does Emacs use -lsystemd?                               yes
  #   Does Emacs use -ljansson?                               yes
  #   Does Emacs use -lgmp?                                   yes
  #   Does Emacs directly use zlib?                           yes
  #   Does Emacs have dynamic modules support?                yes
  #   Does Emacs use toolkit scroll bars?                     yes
  #   Does Emacs support Xwidgets (requires gtk3)?            no
  #   Does Emacs have threading support in lisp?              yes
  #   Does Emacs support the portable dumper?                 yes
  #   Does Emacs support legacy unexec dumping?               no
  #   Which dumping strategy does Emacs use?                  pdumper
  ```

- Ubuntu
  ```dockerfile
  FROM ubuntu:20.04

  MAINTAINER conao3
  
  ENV DEBIAN_FRONTEND=noninteractive
  
  RUN ln -sf /usr/share/zoneinfo/Asia/Tokyo /etc/localtime && \
      sed -ie 's/# deb-src/deb-src/' /etc/apt/sources.list && \
      apt update && apt -y upgrade
  
  ##############################
  
  ARG USER=user
  ARG UID=1000
  ARG GID=1000
  ARG HOME=/home/${USER}
  ARG SHELL=/usr/bin/bash
  
  RUN apt install -y sudo && \
      groupadd -g ${GID} ${USER} && \
      useradd -u ${UID} -g ${GID} -m -s ${SHELL} ${USER} && \
      passwd -d ${USER} && \
      echo "${USER} ALL=(ALL) ALL" > /etc/sudoers && \
      sed -ie 's/^Defaults\tenv_reset/# Defaults\tenv_reset/' /etc/sudoers && \
      sed -ie 's/^Defaults\tsecure_path/# Defaults\tsecure_path/' /etc/sudoers
  
  ##############################
  
  USER ${USER}
  WORKDIR ${HOME}
  
  ENV PATH=/home/user/.local/bin:$PATH
  ENV EDITOR=emacs
  ARG VERSION=27.1
  
  RUN sudo apt install -y curl
  RUN sudo apt install -y postfix
  
  RUN mkdir -p ${HOME}/.local/work && \
      cd ${HOME}/.local/work && \
      curl -LO https://ftp.gnu.org/gnu/emacs/emacs-$VERSION.tar.gz && \
      tar xf emacs-$VERSION.tar.gz
  
  RUN sudo apt install -y build-essential
  RUN sudo apt build-dep -y emacs
  RUN sudo apt install -y libjansson-dev
  
  RUN ls
  RUN cd ${HOME}/.local/work/emacs-$VERSION && \
      ./autogen.sh && \
      ./configure --prefix=$HOME/.local --with-modules && \
      make -j && make install
  
  CMD ["bash"]
  
  # The following NEW packages will be installed:
  #   adwaita-icon-theme autoconf automake autopoint autotools-dev
  #   bsd-mailx bsdmainutils dbus dbus-x11 dconf-gsettings-backend
  #   dconf-service debhelper dh-autoreconf dh-strip-nondeterminism
  #   diffstat dwz fontconfig fontconfig-config fonts-dejavu-core
  #   gettext gettext-base gir1.2-atk-1.0 gir1.2-atspi-2.0
  #   gir1.2-freedesktop gir1.2-gdkpixbuf-2.0 gir1.2-glib-2.0
  #   gir1.2-gtk-3.0 gir1.2-harfbuzz-0.0 gir1.2-pango-1.0
  #   gir1.2-rsvg-2.0 glib-networking glib-networking-common
  #   glib-networking-services groff-base gsettings-desktop-schemas
  #   gtk-update-icon-cache hicolor-icon-theme humanity-icon-theme
  #   icu-devtools imagemagick imagemagick-6-common
  #   imagemagick-6.q16 intltool-debian libacl1-dev libapparmor1
  #   libarchive-zip-perl libasound2 libasound2-data libasound2-dev
  #   libatk-bridge2.0-0 libatk-bridge2.0-dev libatk1.0-0
  #   libatk1.0-data libatk1.0-dev libatspi2.0-0 libatspi2.0-dev
  #   libattr1-dev libavahi-client3 libavahi-common-data
  #   libavahi-common3 libblkid-dev libbsd0 libbz2-dev
  #   libcairo-gobject2 libcairo-script-interpreter2 libcairo2
  #   libcairo2-dev libcolord2 libcroco3 libcups2 libdatrie-dev
  #   libdatrie1 libdbus-1-3 libdbus-1-dev libdconf1
  #   libdebhelper-perl libdjvulibre-dev libdjvulibre-text
  #   libdjvulibre21 libdrm-amdgpu1 libdrm-common libdrm-intel1
  #   libdrm-nouveau2 libdrm-radeon1 libdrm2 libedit2 libegl-dev
  #   libegl-mesa0 libegl1 libegl1-mesa-dev libelf1 libepoxy-dev
  #   libepoxy0 libevent-2.1-7 libexif-dev libexif12 libexpat1-dev
  #   libffi-dev libfftw3-double3 libfile-stripnondeterminism-perl
  #   libfontconfig1 libfontconfig1-dev libfreetype-dev
  #   libfreetype6 libfreetype6-dev libfribidi-dev libfribidi0
  #   libgbm1 libgd3 libgdk-pixbuf2.0-0 libgdk-pixbuf2.0-bin
  #   libgdk-pixbuf2.0-common libgdk-pixbuf2.0-dev libgif-dev
  #   libgif7 libgirepository-1.0-1 libgl-dev libgl1
  #   libgl1-mesa-dev libgl1-mesa-dri libglapi-mesa libgles-dev
  #   libgles1 libgles2 libglib2.0-0 libglib2.0-bin libglib2.0-data
  #   libglib2.0-dev libglib2.0-dev-bin libglvnd-dev libglvnd0
  #   libglx-dev libglx-mesa0 libglx0 libgmp-dev libgmpxx4ldbl
  #   libgnutls-dane0 libgnutls-openssl27 libgnutls28-dev
  #   libgnutlsxx28 libgpm-dev libgpm2 libgraphite2-3
  #   libgraphite2-dev libgtk-3-0 libgtk-3-common libgtk-3-dev
  #   libharfbuzz-dev libharfbuzz-gobject0 libharfbuzz-icu0
  #   libharfbuzz0b libice-dev libice6 libicu-dev libidn2-dev
  #   libilmbase-dev libilmbase24 libjbig-dev libjbig0 libjpeg-dev
  #   libjpeg-turbo8 libjpeg-turbo8-dev libjpeg8 libjpeg8-dev
  #   libjson-glib-1.0-0 libjson-glib-1.0-common liblcms2-2
  #   liblcms2-dev libllvm10 liblockfile-bin liblockfile-dev
  #   liblockfile1 liblqr-1-0 liblqr-1-0-dev libltdl-dev libltdl7
  #   liblzma-dev liblzo2-2 libm17n-0 libm17n-dev
  #   libmagick++-6-headers libmagick++-6.q16-8
  #   libmagick++-6.q16-dev libmagickcore-6-arch-config
  #   libmagickcore-6-headers libmagickcore-6.q16-6
  #   libmagickcore-6.q16-6-extra libmagickcore-6.q16-dev
  #   libmagickwand-6-headers libmagickwand-6.q16-6
  #   libmagickwand-6.q16-dev libmount-dev libncurses-dev
  #   libncurses5-dev libopenexr-dev libopenexr24 libopengl-dev
  #   libopengl0 libotf-dev libotf0 libp11-kit-dev libpango-1.0-0
  #   libpango1.0-dev libpangocairo-1.0-0 libpangoft2-1.0-0
  #   libpangoxft-1.0-0 libpciaccess0 libpcre16-3 libpcre2-16-0
  #   libpcre2-32-0 libpcre2-dev libpcre2-posix2 libpcre3-dev
  #   libpcre32-3 libpcrecpp0v5 libpipeline1 libpixman-1-0
  #   libpixman-1-dev libpng-dev libpng16-16 libproxy1v5
  #   libpthread-stubs0-dev librest-0.7-0 librsvg2-2
  #   librsvg2-common librsvg2-dev libselinux1-dev
  #   libsensors-config libsensors5 libsepol1-dev libsigsegv2
  #   libsm-dev libsm6 libsoup-gnome2.4-1 libsoup2.4-1
  #   libsub-override-perl libsystemd-dev libtasn1-6-dev
  #   libtext-unidecode-perl libthai-data libthai-dev libthai0
  #   libtiff-dev libtiff5 libtiffxx5 libtool libuchardet0
  #   libunbound8 libvulkan1 libwayland-bin libwayland-client0
  #   libwayland-cursor0 libwayland-dev libwayland-egl1
  #   libwayland-server0 libwebp6 libwebpmux3 libwmf-dev
  #   libwmf0.2-7 libx11-6 libx11-data libx11-dev libx11-xcb1
  #   libxau-dev libxau6 libxaw7 libxaw7-dev libxcb-dri2-0
  #   libxcb-dri3-0 libxcb-glx0 libxcb-present0 libxcb-render0
  #   libxcb-render0-dev libxcb-shm0 libxcb-shm0-dev libxcb-sync1
  #   libxcb-xfixes0 libxcb1 libxcb1-dev libxcomposite-dev
  #   libxcomposite1 libxcursor-dev libxcursor1 libxdamage-dev
  #   libxdamage1 libxdmcp-dev libxdmcp6 libxext-dev libxext6
  #   libxfixes-dev libxfixes3 libxft-dev libxft2 libxi-dev libxi6
  #   libxinerama-dev libxinerama1 libxkbcommon-dev libxkbcommon0
  #   libxml-libxml-perl libxml-namespacesupport-perl
  #   libxml-sax-base-perl libxml-sax-perl libxml2 libxml2-dev
  #   libxmu-dev libxmu-headers libxmu6 libxpm-dev libxpm4
  #   libxrandr-dev libxrandr2 libxrender-dev libxrender1
  #   libxshmfence1 libxt-dev libxt6 libxtst-dev libxtst6
  #   libxxf86vm1 m17n-db m4 man-db nettle-dev pango1.0-tools
  #   pkg-config po-debconf python3-distutils python3-lib2to3 quilt
  #   shared-mime-info sharutils tex-common texinfo ubuntu-mono ucf
  #   uuid-dev wayland-protocols x11-common x11proto-core-dev
  #   x11proto-dev x11proto-input-dev x11proto-randr-dev
  #   x11proto-record-dev x11proto-xext-dev x11proto-xinerama-dev
  #   xaw3dg xaw3dg-dev xkb-data xorg-sgml-doctools xtrans-dev
  #   xutils-dev zlib1g-dev
  
  # Configured for 'x86_64-pc-linux-gnu'.
  #   Where should the build process find the source code?    .
  #   What compiler should emacs be built with?               gcc -g3 -O2
  #   Should Emacs use the GNU version of malloc?             no
  #     (The GNU allocators don't work with this system configuration.)
  #   Should Emacs use a relocating allocator for buffers?    no
  #   Should Emacs use mmap(2) for buffer allocation?         no
  #   What window system should Emacs use?                    x11
  #   What toolkit should Emacs use?                          GTK3
  #   Where do we find X Windows header files?                Standard dirs
  #   Where do we find X Windows libraries?                   Standard dirs
  #   Does Emacs use -lXaw3d?                                 no
  #   Does Emacs use -lXpm?                                   yes
  #   Does Emacs use -ljpeg?                                  yes
  #   Does Emacs use -ltiff?                                  yes
  #   Does Emacs use a gif library?                           yes -lgif
  #   Does Emacs use a png library?                           yes -lpng16 -lz
  #   Does Emacs use -lrsvg-2?                                yes
  #   Does Emacs use cairo?                                   no
  #   Does Emacs use -llcms2?                                 yes
  #   Does Emacs use imagemagick?                             no
  #   Does Emacs support sound?                               yes
  #   Does Emacs use -lgpm?                                   yes
  #   Does Emacs use -ldbus?                                  yes
  #   Does Emacs use -lgconf?                                 no
  #   Does Emacs use GSettings?                               yes
  #   Does Emacs use a file notification library?             yes -lglibc (inotify)
  #   Does Emacs use access control lists?                    yes -lacl
  #   Does Emacs use -lselinux?                               yes
  #   Does Emacs use -lgnutls?                                yes
  #   Does Emacs use -lxml2?                                  yes
  #   Does Emacs use -lfreetype?                              yes
  #   Does Emacs use HarfBuzz?                                yes
  #   Does Emacs use -lm17n-flt?                              yes
  #   Does Emacs use -lotf?                                   yes
  #   Does Emacs use -lxft?                                   yes
  #   Does Emacs use -lsystemd?                               yes
  #   Does Emacs use -ljansson?                               yes
  #   Does Emacs use -lgmp?                                   yes
  #   Does Emacs directly use zlib?                           yes
  #   Does Emacs have dynamic modules support?                yes
  #   Does Emacs use toolkit scroll bars?                     yes
  #   Does Emacs support Xwidgets (requires gtk3)?            no
  #   Does Emacs have threading support in lisp?              yes
  #   Does Emacs support the portable dumper?                 yes
  #   Does Emacs support legacy unexec dumping?               no
  #   Which dumping strategy does Emacs use?                  pdumper
  ```

- CentOS 8

  CentOS 8には `m17n-lib-devel` と `libotf-devel` が[削除されている](https://access.redhat.com/documentation/ja-jp/red_hat_enterprise_linux/8/html-single/considerations_in_adopting_rhel_8/index#removed-packages_changes-to-packages)ため、インストールから外しました。
 Configureの表示のとおり、m17nとotfサポートが外れていますが、コンパイル及び正常に起動することを確認しました。
  ```dockerfile
  FROM centos:8
  
  MAINTAINER conao3
  
  ENV LC_ALL=C
  
  RUN dnf -y update && \
      dnf -y install dnf-plugins-core && \
      dnf config-manager --enable AppStream PowerTools
  
  ##############################
  
  ARG USER=user
  ARG UID=1000
  ARG GID=1000
  ARG HOME=/home/${USER}
  ARG SHELL=/usr/bin/bash
  
  RUN dnf install -y sudo passwd && \
      groupadd -g ${GID} ${USER} && \
      useradd -u ${UID} -g ${GID} -m -s ${SHELL} ${USER} && \
      passwd -d ${USER} && \
      echo "${USER} ALL=(ALL) ALL" > /etc/sudoers && \
      sed -ie 's/^Defaults\tenv_reset/# Defaults\tenv_reset/' /etc/sudoers && \
      sed -ie 's/^Defaults\tsecure_path/# Defaults\tsecure_path/' /etc/sudoers
  
  ##############################
  
  USER ${USER}
  WORKDIR ${HOME}
  
  ENV PATH=/home/user/.local/bin:$PATH
  ENV EDITOR=emacs
  ARG VERSION=27.1
  
  RUN mkdir -p ${HOME}/.local/work && \
      cd ${HOME}/.local/work && \
      curl -LO https://ftp.gnu.org/gnu/emacs/emacs-$VERSION.tar.gz && \
      tar xf emacs-$VERSION.tar.gz
  
  RUN sudo dnf -y groupinstall "Development Tools"
  
  RUN curl https://src.fedoraproject.org/rpms/emacs/raw/f33/f/emacs.spec | \
      sed -e 's/m17n-lib-devel//g' -e 's/libotf-devel//g' | \
      grep ^BuildRequires | cut -d" " -f2 | xargs sudo dnf -y install
  
  RUN cd ${HOME}/.local/work/emacs-$VERSION && \
      ./autogen.sh && \
      ./configure --prefix=$HOME/.local --with-modules && \
      make -j && make install
  
  CMD ["bash"]
  
  # Install below packages
  #   alsa-lib-devel atk-devel autoconf bzip2 cairo cairo-devel
  #   dbus-devel desktop-file-utils fontconfig-devel freetype-devel
  #   gcc giflib-devel glibc-devel gnupg2 gnutls-devel gpm-devel
  #   gtk3-devel gzip harfbuzz-devel jansson-devel libacl-devel
  #   libjpeg-turbo libjpeg-turbo-devel liblockfile-devel
  #   libpng-devel librsvg2-devel libselinux-devel libtiff-devel
  #   libX11-devel libXau-devel libXdmcp-devel libxml2-devel
  #   libXpm-devel libXrender-devel libXt-devel ncurses-devel
  #   systemd-devel texinfo util-linux webkit2gtk3-devel
  #   Xaw3d-devel xorg-x11-proto-devel zlib-devel
  
  # Configured for 'x86_64-pc-linux-gnu'.
  #   Where should the build process find the source code?    .
  #   What compiler should emacs be built with?               gcc -g3 -O2
  #   Should Emacs use the GNU version of malloc?             no
  #     (The GNU allocators don't work with this system configuration.)
  #   Should Emacs use a relocating allocator for buffers?    no
  #   Should Emacs use mmap(2) for buffer allocation?         no
  #   What window system should Emacs use?                    x11
  #   What toolkit should Emacs use?                          GTK3
  #   Where do we find X Windows header files?                Standard dirs
  #   Where do we find X Windows libraries?                   Standard dirs
  #   Does Emacs use -lXaw3d?                                 no
  #   Does Emacs use -lXpm?                                   yes
  #   Does Emacs use -ljpeg?                                  yes
  #   Does Emacs use -ltiff?                                  yes
  #   Does Emacs use a gif library?                           yes -lgif
  #   Does Emacs use a png library?                           yes -lpng16 -lz 
  #   Does Emacs use -lrsvg-2?                                yes
  #   Does Emacs use cairo?                                   no
  #   Does Emacs use -llcms2?                                 no
  #   Does Emacs use imagemagick?                             no
  #   Does Emacs support sound?                               yes
  #   Does Emacs use -lgpm?                                   yes
  #   Does Emacs use -ldbus?                                  yes
  #   Does Emacs use -lgconf?                                 no
  #   Does Emacs use GSettings?                               yes
  #   Does Emacs use a file notification library?             yes -lglibc (inotify)
  #   Does Emacs use access control lists?                    yes -lacl
  #   Does Emacs use -lselinux?                               yes
  #   Does Emacs use -lgnutls?                                yes
  #   Does Emacs use -lxml2?                                  yes
  #   Does Emacs use -lfreetype?                              yes
  #   Does Emacs use HarfBuzz?                                yes
  #   Does Emacs use -lm17n-flt?                              no
  #   Does Emacs use -lotf?                                   no
  #   Does Emacs use -lxft?                                   yes
  #   Does Emacs use -lsystemd?                               yes
  #   Does Emacs use -ljansson?                               yes
  #   Does Emacs use -lgmp?                                   yes
  #   Does Emacs directly use zlib?                           yes
  #   Does Emacs have dynamic modules support?                yes
  #   Does Emacs use toolkit scroll bars?                     yes
  #   Does Emacs support Xwidgets (requires gtk3)?            no
  #   Does Emacs have threading support in lisp?              yes
  #   Does Emacs support the portable dumper?                 yes
  #   Does Emacs support legacy unexec dumping?               no
  #   Which dumping strategy does Emacs use?                  pdumper
  ```
