CSS=css/site.css
IDX=index.html
APP=js/app.js
ZIP=miniCast.zip

LDR=css/loaders.css/loaders.min.css
FA=css/Font-Awesome/css/font-awesome.min.css

all: $(APP) $(CSS) $(IDX) $(LDR) $(FA)

$(CSS): resources/public/css/site.css
	mkdir -p css
	cp $< $@

$(APP): src/**/** $(XTRN) project.clj
	rm -f $(APP)
	lein cljsbuild once min

$(IDX): src/clj/minicast/*.clj
	lein run -m minicast.utils/index-html > $(IDX)

$(LDR): resources/public/css/loaders.css/loaders.min.css
	mkdir -p css/loaders.css/
	cp $< $@

$(FA): resources/public/css/Font-Awesome/css/font-awesome.min.css
	mkdir -p css/Font-Awesome/css
	cp $< $@
	cp -av resources/public/css/Font-Awesome/fonts css/Font-Awesome/fonts

$(ZIP): server/index.php $(IDX) $(APP) $(FA) $(LDR) $(CSS) css
	zip -r miniCast.zip $^

clean:
	rm -rf $(CSS) $(APP) $(IDX) $(LDR) $(FA) css js
