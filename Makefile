include version.mk

all: compile


compile: 
	@find . -name '._*' -delete
	[ -d deps/alsa ] || ./rebar get-deps
	./rebar compile

clean:
	./rebar clean

get-deps:
	./rebar get-deps

start:
	run_erl -daemon /tmp/ /tmp/ ./start


attach:
	to_erl /tmp/

image:
	rm -rf root
	mkdir -p root
	make DESTROOT=root install
	mksquashfs root publisher.squashfs 

install:
	mkdir -p $(DESTROOT)/opt/erlyvideo/lib/
	cp -r deps/gproc deps/gen_leader deps/edown $(DESTROOT)/opt/erlyvideo/lib/
	mkdir -p $(DESTROOT)/opt/erlyvideo/lib/publisher-$(VERSION)/priv/
	cp -r apps/publisher/ebin $(DESTROOT)/opt/erlyvideo/lib/publisher-$(VERSION)/
	cp publisher.erl $(DESTROOT)/opt/erlyvideo/lib/publisher-$(VERSION)/priv/
	mkdir -p $(DESTROOT)/etc/service $(DESTROOT)/etc/sv
	cp -r runit $(DESTROOT)/etc/sv/publisher
	ln -sf ../sv/publisher $(DESTROOT)/etc/service/publisher
	

package: compile
	rm -rf tmproot
	mkdir -p tmproot/opt/erlyvideo/lib/publisher-$(VERSION)/priv/
	cp -r apps/publisher/ebin tmproot/opt/erlyvideo/lib/publisher-$(VERSION)/
	cp publisher.erl tmproot/opt/erlyvideo/lib/publisher-$(VERSION)/priv/
	cp -r runit tmproot/opt/erlyvideo/lib/publisher-$(VERSION)/priv/
	mkdir -p tmproot/etc/service/
	ln -s ../../opt/erlyvideo/lib/publisher-$(VERSION)/priv/runit tmproot/etc/service/publisher
	cd tmproot && \
	fpm -s dir -t deb -n publisher -d erlyvideo -d erly-h264 -d erly-alsa -d erly-uvc -d erly-jpeg -v $(VERSION) -m "Max Lapshin <max@maxidoors.ru>" opt etc
	mv tmproot/*.deb .
	rm -rf tmproot


.PHONY: start attach package

