
mkdir root
#mkdir root/bin
#cp -L /bin/sh root/bin/
mkdir root/lib
mkdir root/lib/x86_64-linux-gnu
cp -L /lib/x86_64-linux-gnu/libc.so.6 root/lib/x86_64-linux-gnu/
cp -L /lib/x86_64-linux-gnu/libdl.so.2 root/lib/x86_64-linux-gnu/
cp -L /lib/x86_64-linux-gnu/libm.so.6 root/lib/x86_64-linux-gnu/
cp -L /lib/x86_64-linux-gnu/libpthread.so.0 root/lib/x86_64-linux-gnu/
cp -L /lib/x86_64-linux-gnu/libutil.so.1 root/lib/x86_64-linux-gnu/
cp -L /lib/x86_64-linux-gnu/librt.so.1 root/lib/x86_64-linux-gnu/
cp -L /lib/x86_64-linux-gnu/libz.so.1 root/lib/x86_64-linux-gnu/
cp -L /lib/x86_64-linux-gnu/libnss_files.so.2 root/lib/x86_64-linux-gnu/
cp -L /lib/x86_64-linux-gnu/libnss_dns.so.2 root/lib/x86_64-linux-gnu/
cp -L /lib/x86_64-linux-gnu/libresolv.so.2 root/lib/x86_64-linux-gnu/

mkdir root/lib64
cp -L /lib64/ld-linux-x86-64.so.2 root/lib64/

#mkdir root/etc
#cp -L /etc/protocols root/etc/
#cp -L /etc/services root/etc/

mkdir root/usr
mkdir root/usr/lib
mkdir root/usr/lib/x86_64-linux-gnu
mkdir root/usr/lib/x86_64-linux-gnu/gconv
cp -L /usr/lib/x86_64-linux-gnu/gconv/UTF-16.so root/usr/lib/x86_64-linux-gnu/gconv/
cp -L /usr/lib/x86_64-linux-gnu/gconv/UTF-32.so root/usr/lib/x86_64-linux-gnu/gconv/
cp -L /usr/lib/x86_64-linux-gnu/gconv/UTF-7.so root/usr/lib/x86_64-linux-gnu/gconv/
cp -L /usr/lib/x86_64-linux-gnu/gconv/gconv-modules root/usr/lib/x86_64-linux-gnu/gconv/
cp -L /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache root/usr/lib/x86_64-linux-gnu/gconv/
cp -L /usr/lib/x86_64-linux-gnu/libgmp.so.10 root/usr/lib/x86_64-linux-gnu/