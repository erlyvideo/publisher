# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant::Config.run do |config|


  config.vm.define :squeeze64 do |conf|
    conf.vm.box = "squeeze64"
    conf.vm.box_url = "http://puppetlabs.s3.amazonaws.com/pub/squeeze64.box"
    conf.vm.provision :shell, :path => "priv/squeeze64.sh"
    conf.vm.forward_port 8080, 9180
  end


end
