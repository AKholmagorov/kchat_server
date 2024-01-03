kchat_server
=====

Step 0. Change path
-----
In file my_jwt.erl change path (usually not required and works 'as is')

Step 1. Install Erlang/OTP (v. 22 or higher)                                    
-----
    $ sudo apt-get install erlang

Step 2. Install rebar3                                    
-----
    $ git clone https://github.com/erlang/rebar3.git
    $ cd rebar3
    $ ./bootstrap
    $ sudo mv rebar3 /usr/local/bin
    
Step 3. Install MySQL
-----
    $ sudo apt install mysql-server
    
After installation start it:

    $ sudo systemctl start mysql.service
    
Step 4. Import DB dump
-----
    $ mysql -u root -p
    $ CREATE DATABASE k_db;
    $ EXIT;
    $ mysql -u root -p k_db < /path/to/k_db.sql

If you got error like "Access denied for user 'root'@'localhost'"

    $ mysql -u root -p
    $ ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY 'root';
    $ FLUSH PRIVILEGES;
    $ EXIT;

Step 5. Launch
-----
Go to the root of the project and execute follow:

    $ rebar3 shell
    $ main:start().

