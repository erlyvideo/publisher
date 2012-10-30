# -*- mode: ruby -*-
# vi: set ft=ruby :

`mkdir -p .vagrant_aptcache`

Vagrant::Config.run do |conf|

  conf.vm.box = "squeeze64"
  conf.vm.box_url = "http://puppetlabs.s3.amazonaws.com/pub/squeeze64.box"
  conf.vm.provision :shell, :path => "squeeze64.sh"
  conf.vm.forward_port 8080, 9180

  conf.vm.share_folder "v-apt", "/var/cache/apt", ".vagrant_aptcache"
end
