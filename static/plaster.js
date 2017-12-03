var Plaster = function(){
    var self = this;

    self.loadedModes = ["text"];
    self.loadedThemes = ["default"];
    self.staticUrl = document.getElementById("static-codemirror-root").href;
    self.editors = [];
    self.defaultConfig = {"lineNumbers": true,
                          "lineWrapping": true,
                          "viewportMargin": Infinity,};

    self.log = function(){
        var args = Array.prototype.slice.call(arguments);
        args.unshift("[Plaster]");
        console.log.apply(console, args);
        return null;
    }

    self.addToHead = function(element){
        document.getElementsByTagName("head")[0].appendChild(element);
        return element;
    }

    self.loadScript = function(url, callback){
        self.log("Loading script",url);
        var script = document.createElement("script");
        script.type = "text/javascript";
        script.src = url;
        script.onload = callback;
        return self.addToHead(script);
    }

    self.loadStylesheet = function(url, callback){
        self.log("Loading stylesheet",url);
        var link = document.createElement("link");
        link.type = "text/css";
        link.rel = "stylesheet";
        link.href = url;
        link.onload = callback;
        return self.addToHead(link);
    }

    self.maybeLoadTheme = function(theme, callback){
        if(self.loadedThemes.indexOf(theme) === -1){
            self.loadedThemes.push(theme);
            self.loadStylesheet(self.staticUrl + "theme/"+theme+".css", callback);
        }else{
            if(callback) callback();
        }
        return true;
    }

    self.maybeLoadMode = function(mode, callback){
        if(self.loadedModes.indexOf(mode) === -1){
            self.loadedModes.push(mode);
            self.loadScript(self.staticUrl + "mode/"+mode+".js", callback);
        }else{
            if(callback) callback();
        }
        return true;
    }

    self.createEditor = function(element, config, callback){
        var text = element.getElementsByClassName("text")[0];
        var type = element.getElementsByClassName("type")[0];
        var theme = element.getElementsByClassName("theme")[0];
        var textarea;

        if(text.tagName !== "textarea"){
            textarea = document.createElement("textarea");
            for(var i=0; i<text.attributes.length; i++){
                textarea.setAttribute(text.attributes[i].name, text.attributes[i].value);
            }
            textarea.innerHTML = text.innerHTML;
            text.parentNode.replaceChild(textarea, text);
        }else{
            textarea = text;
        }

        self.editors.push(element);
        if(!config) config = {};
        if(!config.readOnly) config.readOnly = text.hasAttribute("readonly");

        if(config.theme){
        }else if(!theme){
            config.theme = "default";
        }else if(theme.tagName === "select"){
            config.theme = theme.options[theme.selectedIndex].value;
            theme.addEventListener("change", function(){
                self.changeTheme(element, theme.options[theme.selectedIndex].value);
            }, false);
        }else{
            config.theme = theme.textContent;
        }

        if(config.mode){
        }else if(!type){
            config.mode = "text";
        }else if(type.tagName === "select"){
            config.mode = type.options[type.selectedIndex].value;
            type.addEventListener("change", function(){
                self.changeMode(element, type.options[type.selectedIndex].value);
            }, false);
        }else{
            config.mode = type.textContent;
        }

        self.log("Creating mirror for",element,"with config",config);
        element.mirror = null;
        self.maybeLoadTheme(config.theme, function(){
            self.maybeLoadMode(config.mode, function(){
                textarea.removeAttribute("required");
                element.mirror = CodeMirror.fromTextArea(textarea, config);
                if(callback) callback(element);
            });
        });
        return element;
    }

    self.maybeCreateEditor = function(element, config, callback){
        if(self.editors.indexOf(element) === -1){
            self.createEditor(element, config, callback);
        }else{
            if(callback) callback();
        }
        return element;
    }

    self.changeMode = function(element, mode){
        if(!element.mirror) throw element+" is not an initialized CodeMirror editor.";
        self.maybeLoadMode(mode, function(){
            element.mirror.setOption("mode", mode);
        });
        return element;
    }

    self.changeTheme = function(element, theme){
        if(!element.mirror) throw element+" is not an initialized CodeMirror editor.";
        self.maybeLoadTheme(theme, function(){
            element.mirror.setOption("theme", theme);
        });
        return element;
    }

    self.initEditors = function(){
        var els = document.getElementsByClassName("edit");

        var createNext = function(i){
            if(i<els.length)
                self.maybeCreateEditor(els[i], self.defaultConfig, function(){createNext(i+1)});
        }
        
        createNext(0);
        return els;
    }

    self.init = function(){
        self.initEditors();
        // Make sure to jump after editors have been created.
        // The initial anchor won't work otherwise.
        var hash = location.hash;
        location.hash = "";
        location.hash = hash;
        return true;
    }
}

var plaster = new Plaster();
window.onload = plaster.init
