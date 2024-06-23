![Ipify4Pascal](https://github.com/rafael-figueiredo-alves/Ipify4Pascal/assets/58051735/14973afb-dde7-4176-bb6a-7d1aaa384edb)

# Ipify4Pascal
 An unofficial Pascal library (Delphi and Lazarus) for [Ipify.org](https://www.ipify.org/), an API that lets you easily get the IPv4 and IPv6 from Client. It can be very useful for those front-end projects that need or want to get the client's IP and send it to the server. It's a very small and easy-to-use library. To get the IP, you simply need to make the following call:

    ipify4Pascal.GetIPv4; // Let you get the IPv4
    ipify4Pascal.GetIpv6; // Let you get the IPv6

Don't foreget to declare on the uses section of your project the unit **LibIpify4Pascal**.

So, to be able to get started, you can simply download the zip, copy the LibIpify4Pascal.pas from /src folder and add it to your project. But I'd always recomend to use [Boss](https://github.com/HashLoad/boss) to install it on your project. To do so, open your project's folder, open the terminal and run the following commands (be sure you have [Boss](https://github.com/HashLoad/boss/releases) installed):

    boss init

After typing this command, you have to answer the questions that will be prompted abour your project. When you finish, use the following command:

    boss install https://github.com/rafael-figueiredo-alves/Ipify4Pascal

Finally, you'll be all set to begin.
