# Nginx

## Install

    sudo aptitude install nginx

## Control

    sudo /etc/init.d/nginx start
    sudo /etc/init.d/nginx stop
    sudo /etc/init.d/nginx restart

## Configuration

### General 

Follow [these instructions](http://articles.slicehost.com/2009/3/5/ubuntu-intrepid-nginx-configuration)
to edit default configs slighly.

These configs are located at `/etc/nginx/nginx.conf`

### Yesod specific

Follow [these](http://www.fatvat.co.uk/2011/06/deploying-yesod-application-on-linode.html) instructions or the ones
given on the [deploy chapter](http://www.yesodweb.com/book/deploying-your-webapp) of the Yesod Book.
