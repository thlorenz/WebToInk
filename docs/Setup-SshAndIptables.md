# Config SSH

Follow steps given [here](http://articles.slicehost.com/2010/10/18/ubuntu-maverick-setup-part-1)

## Local Station

    mkdir ~/.ssh
    ssh-keygen -t rsa

    scp ~/.ssh/id_rsa.pub user@target:

## Remote Machine

    sudo mkdir ~user/.ssh
    sudo mv ~user/id_rsa.pub ~user/.ssh/authorized_keys

    sudo chown -R user:user ~user/.ssh
    sudo chmod 700 ~user/.ssh
    sudo chmod 600 ~user/.ssh/authorized_keys

### SSH config
    
Change port in config:

    sudo vim /etc/ssh/sshd_config 

# iptables

## Show Rules

    sudo iptables -L

## Config 

Download template and adjust port to match ssh port
    
    sudo wget http://articles.slicehost.com/assets/2007/9/4/iptables.txt

    sudo mv ipatbles.txt /etc/iptables.up.rules

Tell system to load iptables on startup

    sudo vim /etc/network/if-pre-up.d/iptables

Enter:
    
    #!/bin/sh
    /sbin/iptables-restore < /etc/iptables.up.rules

Make script executable

    sudo chmod +x /etc/network/if-pre-up.d/iptables

### Edit iptables later

    sudo vim /etc/iptables.up.rules
    sudo iptables -F
    sudo iptables-restore < /etc/iptables.up.rules

### Local Station

    ssh -p port user@url
