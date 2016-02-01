Vagrant.configure(2) do |config|
  config.vm.box = "debian/jessie64"
  config.vm.provision "shell", inline: <<-SHELL
    sudo apt-get update
    sudo apt-get install -y make sbcl libmount-dev
    cd /vagrant
    make
    sudo make install
  SHELL
end
