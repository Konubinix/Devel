#!/bin/bash
#
printf "\033[32m##########Configuration d'iptables\n\033[0m"

#Périphérique connecté au net
periph_net=eth0
#Périphérique de réseau local
#periph_res_local=eth0

function init(){
# Nous vidons les chaines predefinies :
    iptables -F
    iptables -t nat -F
    iptables -t mangle -F
    
# Nous supprimons les regles des chaines personnelles :
    iptables -X 
    iptables -t nat -X
    iptables -t mangle -X
    
# Politique d'acceptation du trafic sortant uniquement
    iptables -P INPUT ACCEPT
    iptables -P OUTPUT ACCEPT
    iptables -P FORWARD ACCEPT

#init des tables NAT et MANGLE (pas forcément nécessaire)
    iptables -t nat -F
    iptables -t nat -X
    iptables -t nat -P PREROUTING ACCEPT
    iptables -t nat -P POSTROUTING ACCEPT
    iptables -t nat -P OUTPUT ACCEPT
    
    iptables -t mangle -F
    iptables -t mangle -X
    iptables -t mangle -P PREROUTING ACCEPT
    iptables -t mangle -P OUTPUT ACCEPT
}

function param_localhost(){
##################################################
#Config lo
##################################################
    
#lo est sur, mais il faut prendre ceux qui sont bons quand même
    iptables -A INPUT -i lo  -m state --state ! INVALID -j ACCEPT
#iptables -A OUTPUT -o lo  -m state --state ! INVALID -j ACCEPT
}

function param_net(){
    periph_net=$1
##################################################
#Config périph net
##################################################
#Création de chaînes pour le périph
    iptables -N ${periph_net}_INPUT
#iptables -N ${periph_net}_OUTPUT
    
#Transfert des règles générales vers celles du périph
    iptables -A INPUT -i $periph_net -j ${periph_net}_INPUT
#iptables -A OUTPUT -o $periph_net -j ${periph_net}_OUTPUT
    
#Autorise les réponses
    iptables -A ${periph_net}_INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT -i $periph_net
#iptables -A ${periph_net}_OUTPUT -m state --state ESTABLISHED,RELATED -j ACCEPT -o $periph_net
    
# On autorise le ping
#iptables -A ${periph_net}_OUTPUT -p icmp -m state --state NEW -j ACCEPT -o $periph_net
#     iptables -A ${periph_net}_INPUT -p icmp -m state --state NEW -j ACCEPT -i $periph_net
#     iptables -A ${periph_net}_INPUT -p icmp -m limit --limit 5/s -j ACCEPT -i $periph_net
    
# # anti scan
# #iptables -A ${periph_net}_INPUT -i eth0 -p tcp --tcp-flags FIN,URG,PSH FIN,URG,PSH -j DROP
# #iptables -A ${periph_net}_INPUT -i eth0 -p tcp --tcp-flags ALL ALL -j DROP
# #iptables -A ${periph_net}_INPUT -i eth0 -p tcp --tcp-flags ALL NONE -j DROP
# #iptables -A ${periph_net}_INPUT -i eth0 -p tcp --tcp-flags SYN,RST SYN,RST -j DROP
# #iptables -A ${periph_net}_INPUT -i eth0 -p tcp --tcp-flags SYN,ACK,FIN,RST RST -j DROP
    
# #Make sure NEW incoming tcp connections are SYN packets
#     iptables -A ${periph_net}_INPUT -p tcp ! --syn -m state --state NEW -j DROP -i $periph_net
    
# #Packets with incoming fragments
#     iptables -A ${periph_net}_INPUT -f -j DROP -i $periph_net

# #incoming malformed XMAS packets
#     iptables -A ${periph_net}_INPUT -p tcp --tcp-flags ALL ALL -j DROP -i $periph_net

# #Incoming malformed NULL packets
#     iptables -A ${periph_net}_INPUT -p tcp --tcp-flags ALL NONE -j DROP -i $periph_net

# #Autoriser DNS
#     iptables -A ${periph_net}_INPUT --protocol udp --source-port 53 -j ACCEPT -i $periph_net
# #iptables -A ${periph_net}_OUTPUT --protocol udp --destination-port 53 -j ACCEPT -o $periph_net

# #Autoriser serveur FTP
# #iptables -A ${periph_net}_INPUT -i ${periph_net} -p tcp --dport ftp -m state --state ! INVALID -j ACCEPT
# #iptables -A ${periph_net}_INPUT -i ${periph_net} -p tcp --sport ftp-data -m state --state ESTABLISHED,RELATED -j ACCEPT

# #Autoriser serveur SSH
# #iptables -A ${periph_net}_INPUT -i ${periph_net} -p tcp --dport ssh -m state --state ! INVALID -j ACCEPT
# #iptables -A ${periph_net}_OUTPUT -o ${periph_net} -p tcp --sport ssh -m state --state ESTABLISHED,RELATED -j ACCEPT

# #Autoriser serveur HTTP
# #iptables -A ${periph_net}_INPUT -i ${periph_net} -p tcp --dport http -m state --state ! INVALID -j ACCEPT
# #iptables -A ${periph_net}_OUTPUT -o ${periph_net} -p tcp --sport http -m state --state ESTABLISHED,RELATED -j ACCEPT

# #Autoriser serveur HTTPS
# #iptables -A ${periph_net}_INPUT -i ${periph_net} -p tcp --dport https -m state --state ! INVALID -j ACCEPT
# #iptables -A ${periph_net}_OUTPUT -o ${periph_net} -p tcp --sport https -m state --state ESTABLISHED,RELATED -j ACCEPT

# #Autorisation DHCP
    iptables -A ${periph_net}_INPUT  -i ${periph_net} -s 0.0.0.0 -d 255.255.255.255 -p udp --sport 68 --dport 67 -j ACCEPT

# #Autoriser partage torrent
#     iptables -A  ${periph_net}_INPUT -i ${periph_net} -p tcp --dport 6881 -j ACCEPT

##########Autoriser connexion VPN
#    iptables -A INPUT -i tun+ -j ACCEPT

    # rejete statcounter
    iptables -A ${periph_net}_INPUT  -i ${periph_net} -s 0.0.0.0 -d 255.255.255.255 -p udp --sport 68 --dport 67 -j ACCEPT

}

function log(){
    periph_net=$1
#LOG des paquets rejétés
    iptables -A ${periph_net}_INPUT -p all -j LOG --log-prefix "ENTRANT_REJ_IPTABLES " -i $periph_net
#iptables -A ${periph_net}_OUTPUT -p all -j LOG --log-prefix "SORTANT_REJ_IPTABLES " -o $periph_net
#-m limit --limit 1/second
}

function param_local(){
    periph_res_local=$1
####################Règles du réseau local (sécurisé)
#Création de chaînes pour le périph
    iptables -N ${periph_res_local}_INPUT
#iptables -N ${periph_res_local}_OUTPUT

#Transfert des règles générales vers celles du périph
    iptables -A INPUT -i $periph_res_local -j ${periph_res_local}_INPUT
#iptables -A OUTPUT -o $periph_res_local -j ${periph_res_local}_OUTPUT

#Le réseau est sûr en général
    iptables -A ${periph_res_local}_INPUT -p all -j ACCEPT -i $periph_res_local
#iptables -A ${periph_res_local}_OUTPUT -p all -j ACCEPT -o $periph_res_local
}

function partage_vpn(){
##########Partage connexion net sur vpn
    echo 1 >/proc/sys/net/ipv4/ip_forward
    iptables -t nat -A POSTROUTING -o tun0 -j MASQUERADE
    iptables -A FORWARD -j ACCEPT
}

function redirections_par_tunnel(){
# # Pour rediriger les connexion cgoban vers localhost, à combiner avec le tunnel
# # ssh -L 2379:74.52.20.154:2379 lourys@ensibm.imag.fr
# # Pour jouer
#     iptables -t nat -A OUTPUT -d 74.52.20.154 -p tcp --dport 2379 -j DNAT --to-destination 127.0.0.1
    
# #Pour jeu d'echec FICS
# #a combiner avec ssh -L 5000:69.36.243.188:5000 lourys@ensibm.imag.fr
# #    iptables -t nat -A OUTPUT -d 69.36.243.188 -p tcp --dport 5000 -j DNAT --to-destination 127.0.0.1

# #Pour toribash
# #a combiner avec ssh -L tout ça lourys@ensibm.imag.fr
# # Il faut en faire plein de 22000 à 22999 au moins
# # iptables -t nat -A OUTPUT -d 66.230.225.250 -p tcp --dport 22000 -j DNAT --to-destination 127.0.0.1

# #Connexion google talk
# #A combiner avec ssh -L tout ça lourys@ensibm.imag.fr
#     iptables -t nat -A OUTPUT -d 209.85.137.125 -p tcp --dport 5222 -j DNAT --to-destination 127.0.0.1

# #Connexion smtp ensibm
# #A combiner avec ssh -L tout ça lourys@ensibm.imag.fr
# #    iptables -t nat -A OUTPUT -d ensibm.imag.fr -p tcp --dport 25 -j DNAT --to-destination 127.0.0.1

# #Connexion smtp gmail
# #A combiner avec ssh -L tout ça lourys@ensibm.imag.fr
#     iptables -t nat -A OUTPUT -d smtp.gmail.com -p tcp --dport 587 -j DNAT --to-destination 127.0.0.1

# #Connexion IRC ubuntu et efnet.fr
# #A combiner avec ssh -L tout ça lourys@ensibm.imag.fr
#     iptables -t nat -A OUTPUT -d irc.ubuntu.com -p tcp --dport 8001 -j DNAT --to-destination 127.0.0.1
     iptables -t nat -A OUTPUT -d irc.efnet.fr -p tcp --dport 6667 -j DNAT --to-destination 127.0.0.1
#     iptables -t nat -A OUTPUT -d irc.freenode.net -p tcp --dport 8001 -j DNAT --to-destination 127.0.0.1

# #Connexion CLEF UBUNTU LAUNCHPAD
# #A combiner avec ssh -L tout ça lourys@ensibm.imag.fr
# #    iptables -t nat -A OUTPUT -d 91.189.94.173 -p tcp --dport 11371 -j DNAT --to-destination 127.0.0.1
    
 #Connexion SMTP CVDM-SOLUTIONS
    #iptables -t nat -A OUTPUT -d 213.186.33.20 -p tcp --dport 25 -j DNAT --to-destination 127.0.0.1
echo "redirect"
}

init

# param_net $periph_net

# log $periph_net

# param_local $periph_res_local

# param_localhost

# partage_vpn

redirections_par_tunnel