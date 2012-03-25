mkdir fcgi
cd fcgi
wget http://www.fastcgi.com/dist/fcgi.tar.gz
tar -zxvf fcgi.tar.gz

cd fcgi-2.4.0 
./configure
make install

cd ../../
rm -rf fcgi
