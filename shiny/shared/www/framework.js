$(document).ready(function() {
/*  ------------------------------------------------------------------------
    dynamically resize content wrapper to allow it to carry the scrollbar (i.e. header stays fixed)
    ------------------------------------------------------------------------*/    
    let setCWHeight = function(){
        $(".content-wrapper").height($(window).height() - 200);
    };
    setCWHeight();
    $(window).resize(setCWHeight);

/*  ------------------------------------------------------------------------
    activate page reset
    ------------------------------------------------------------------------*/ 
    $(".main-header .logo").on('click', function(){
        Shiny.setInputValue('resetPage', true, {priority: "event"}); 
    });

/*  ------------------------------------------------------------------------
    capture external web links into desktop app
    ------------------------------------------------------------------------*/ 
    if(window.mdiElectron && window.mdiElectron.frameworkToElectron){
        $(document).on("click", "a", function(event){
            const linkTarget = $(event.target).attr('target');
            if(linkTarget){ // all external links are expected to set a target
                const url = $(event.target).attr('href');
                if(!url.includes("/download/")){ // don't mask Save Bookmark
                    event.preventDefault(); // TODO: this is preventing Save Bookmark from functioning
                    window.mdiElectron.frameworkToElectron("externalLink", {
                        url: url,
                        target: linkTarget
                    });
                }
            }
        });
    }
});

/*  ------------------------------------------------------------------------
    enable cookies for storing user and session history information
    ------------------------------------------------------------------------*/
function getCookie(cname) {
  let name = cname + "=";
  let decodedCookie = decodeURIComponent(document.cookie);
  let ca = decodedCookie.split(';');
  for(var i = 0; i <ca.length; i++) {
    let c = ca[i];
    while (c.charAt(0) == ' ') {
      c = c.substring(1);
    }
    if (c.indexOf(name) === 0) {
      return c.substring(name.length, c.length);
    }
  }
  return "";
}
function setCookie(cname, data, nDays) {
    let currentValue = getCookie(cname);
    if (currentValue === "" || data.force) currentValue = data.value;
    let secure = data.isServerMode ? ";secure" : ""; // secure means transmit over https only
    if (nDays === undefined) { // a session cookie; note: cannot set HttpOnly in javascript
        document.cookie = cname + "=" + currentValue + ";path=/;samesite=lax" + secure;
    } else { // a permanent cookie
        var d = new Date();
        d.setTime(d.getTime() + (nDays * 24 * 60 * 60 * 1000));
        var expires = "expires="+ d.toUTCString();
        document.cookie = cname + "=" + currentValue + ";path=/;samesite=lax" + secure + ";" + expires;
    }
}
// user and session keys (use maximum possible security)
Shiny.addCustomMessageHandler('initializeSession', function(data) { 
    let priorCookie = decodeURIComponent(document.cookie);
    setCookie('hostKey', data, 10 * 365);
    if (!data.isServerMode){
        setCookie('sessionKey', data); // isSession is a safe flag for the existence of potentially invisible sessionKey
        setCookie('isSession',  {value: 1, force: true, isServerMode: data.isServerMode});
    }
    let cookie = decodeURIComponent(document.cookie);
    let sessionNonceElement = document.getElementById('sessionNonce');
    let sessionNonce = sessionNonceElement.value;
    sessionNonceElement.remove(); // sessionNonce is a one-time sessionKey lookup passed from ui.R to server.R
    Shiny.setInputValue(
        'initializeSession',
        {priorCookie: priorCookie, cookie: cookie, sessionNonce: sessionNonce},
        {priority: "event"}
    );
});

// any generic cookie, e.g., app usage history (low security level here)
Shiny.addCustomMessageHandler('setDocumentCookie', function(cookie) { // Shiny to javascript
    cookie.data.force = true;    
    setCookie(cookie.name, cookie.data, cookie.nDays);
});
Shiny.addCustomMessageHandler('setCookieInput', function(cookieName) { // javascript to Shiny
    var decodedCookie = decodeURIComponent(document.cookie);
    Shiny.setInputValue(cookieName, decodedCookie, {priority: "event"});
});

/*  ------------------------------------------------------------------------
    accept instructions from the server to act as triggers for conditionalPanels
    ------------------------------------------------------------------------*/
window.stepIsReady = { NO_SOURCE: true };
Shiny.addCustomMessageHandler('updateTrigger', function(trigger) {
    window[trigger.name] = trigger.value;
});
Shiny.addCustomMessageHandler('updateTriggerArray', function(trigger) {
    window[trigger.name][trigger.index] = trigger.value;
});

/*  ------------------------------------------------------------------------
    help Shiny show a spinner and mask elements on certain slow actions
    ------------------------------------------------------------------------*/
Shiny.addCustomMessageHandler('toggleSpinner', function(options) {
    $("#mainSpinner-message").html(options.message || "");    
    $(".mdi-main-spinner").css('visibility', options.visibility);
});
Shiny.addCustomMessageHandler('maskElement', function(options) {
    $("#" + options.id).css('opacity', options.masked === true ? 0.5 : 1);
});

/*  ------------------------------------------------------------------------
    help Shiny show a spinner and mask elements on certain slow actions
    ------------------------------------------------------------------------*/
Shiny.addCustomMessageHandler('addMdiTooltip', function(data) {
    $("#" + data.id).tooltip(data.options);
});

/*  ------------------------------------------------------------------------
    handle Ace Code Editor
    ------------------------------------------------------------------------*/
let initializeAceCodeEditor = function(editorId, readOnly, mode = "r"){
    window[editorId] = ace.edit(editorId);    
    window[editorId].setTheme("ace/theme/crimson_editor");
    window[editorId].session.setMode("ace/mode/" + mode);
    window[editorId].setReadOnly(readOnly);
    if(!readOnly) window[editorId].commands.addCommand({
        name: 'save',
        bindKey: { win: "Ctrl-S", mac: "Cmd-S" },
        exec: function(editor) {
            saveAceSessionContents(editorId, aceCurrentPath, 'save');
        }
    })    
}
Shiny.addCustomMessageHandler('initializeAceCodeEditor', function(editorId) {
    initializeAceCodeEditor(editorId, false);
});
Shiny.addCustomMessageHandler('initializeAceCodeReader', function(editorId) {
    initializeAceCodeEditor(editorId, true);
});
// getAceCodeContents and setAceCodeContents are for older, single-session editors
Shiny.addCustomMessageHandler('getAceCodeContents', function(options) {
    let code = window[options.editorId].getValue();
    Shiny.setInputValue(options.editorId + "-contents", 
                        {file: options.file, code: code, flag: options.flag}, 
                        {priority: "event"});
});
Shiny.addCustomMessageHandler('setAceCodeContents', function(options) {
    window[options.editorId].session.setValue(options.code);
});
// the following are for more current usage of mutli-session editors
let aceSessionModes = {
    yml: "ace/mode/yaml",
    R:   "ace/mode/r",
    md:  "ace/mode/markdown"
};
let aceTabs = {};
let aceCurrentPath = "";
let initializeAceSession = function(options){
    let ext = options.path.split('.').pop();
    let mode = aceSessionModes[ext];
    if(mode === undefined) mode = aceSessionModes.R;
    if(aceTabs[options.path] === undefined || options.force === true){
        let session = ace.createEditSession(options.contents, mode);
        aceTabs[options.path] = {
            disk: options.contents,
            session: session
        };
        session.on('change', function(delta) {
            Shiny.setInputValue(
                options.editorId + "-changed", 
                {
                    path: options.path, 
                    changed: session.getValue() !== aceTabs[options.path].disk
                }, 
                { priority: "event" }
            );
        });
    }
    aceCurrentPath = options.path;
    window[options.editorId].setSession(aceTabs[options.path].session);
}
Shiny.addCustomMessageHandler('initializeAceSession', function(options) {
    initializeAceSession(options)
});
Shiny.addCustomMessageHandler('clearAceSession', function(editorId) {
    let mode = aceSessionModes.R;
    let session = ace.createEditSession("", mode);
    aceCurrentPath = "";
    window[editorId].setSession(session);
});
let resetSessionContents = function(editorId, path){
    initializeAceSession({
        editorId: editorId,
        path: path,
        contents: aceTabs[path].disk,
        force: true
    })
    Shiny.setInputValue(
        editorId + "-discard", 
        aceTabs[path].disk, 
        { priority: "event" }
    );
}
let saveAceSessionContents = function(editorId, path, action){
    if(path === "") return;
    let tab = aceTabs[path];
    Shiny.setInputValue(
        editorId + "-contents", 
        {
            path: path, 
            contents: tab === undefined ? undefined : tab.session.getValue(),
            action: action
        }, 
        { priority: "event" }
    );
}
Shiny.addCustomMessageHandler('terminateAceSession', function(options) {
    delete aceTabs[options.closingPath];
    if(options.newPath === null){
        aceCurrentPath = "";
        window[options.editorId].setSession(ace.createEditSession("", aceSessionModes.R));
    } else {
        aceCurrentPath = options.newPath;
        window[options.editorId].setSession(aceTabs[options.newPath].session);
    }
});

/*  ------------------------------------------------------------------------
    DT table actions
    ------------------------------------------------------------------------*/
let handleActionClick = function(parentId, instanceId, confirmMessage){ // used by standard table action links
    if(confirmMessage === "NO_CONFIRM" || confirm(confirmMessage) === true){
        let val = instanceId + '__' + Math.floor(Math.random() * 1e6);
        Shiny.setInputValue(parentId, val, {priority: "event"});
    }
};
let handleActionClick2 = function(parentId, instanceId, confirmMessage){ // used by mdiSharedEventHandler, from table action links
    if(confirmMessage === "NO_CONFIRM" || confirm(confirmMessage) === true){
        let val = instanceId + '__' + Math.floor(Math.random() * 1e6);
        Shiny.setInputValue("mdiSharedEventHandler", {key: parentId, val: val}, {priority: "event"});
    }
};
let handleActionClick3 = function(eventKey, eventData){ // used by mdiSharedEventHandler, from standard action links
    Shiny.setInputValue("mdiSharedEventHandler", {key: eventKey, val: eventData}, {priority: "event"});
};
let dtNumericFilters = [];
let setDTColumnFilter = function(tableId, columnI, type, filter){
    let table = $('#' + tableId + " .dataTable");
    const dtId = table.attr('id'); // not the same as tableId, this is set by DataTables
    table = table.DataTable();    
    if(type == "character"){ // datatables handles character column matching
        table.columns(columnI).search(filter);
    } else { // MDI handles numeric column matching via custom search functions instantiated on first use
        const filterId = tableId + "-filter-" + columnI;
        if(!dtNumericFilters[filterId]){
            $.fn.dataTable.ext.search.push(function(settings, data, dataIndex){
                if(dtId !== settings.sTableId) return true; // only apply filters to the table that create them 
                const filter = $("#" + filterId).val().replaceAll(" ", "");
                if(filter === "") return true;
                const i = filter.search(/\d/);
                if(i === -1) return true;
                let operation = i === 0 ? "==" : filter.substring(0, i);
                if(operation === "=") operation = "==";
                try {                
                    const expr = data[columnI] + " " + operation + " " + parseFloat(filter.substring(i));
                    return(eval(expr));
                } catch (error) {
                    return(true);
                }
            }); 
            dtNumericFilters[filterId] = true;
        }
    }
    table.draw();
}

/*  ------------------------------------------------------------------------
    Pipeline Runner, functions to simplify the number of required input observers in R
    ------------------------------------------------------------------------*/
Shiny.addCustomMessageHandler('initializePRCodeEditor', function(editorId) {
    initializeAceCodeEditor(editorId, false, "yaml");
    window[editorId].session.on('change', function(delta) {
        Shiny.setInputValue(
            editorId + "-contents", 
            {contents: window[editorId].getValue()}, 
            {priority: "event"}
        );
    });
});
let prInputOnChange = function(x){ // when any PR text input changes
    let parts = x.id.split('__');
    Shiny.setInputValue(
        parts[0], 
        {id: parts[1], value: x.value, logical: false}, 
        {priority: "event"}
    );
}
let prCheckboxOnChange = function(x){ // when any PR checkbox changes
    let parts = x.name.split('__');
    Shiny.setInputValue(
        parts[0], 
        {id: parts[1], value: $(x).prop("checked"), logical: true}, 
        {priority: "event"}
    );
}
let prAddToList = function(x){ // react to the add/remove list action requests
    let parts = x.split('__');
    Shiny.setInputValue(
        "configure-inputEditor-prAddToList", 
        parts[1], 
        {priority: "event"}
    );
}
let prRemoveLastItem = function(x){ 
    let parts = x.split('__');
    Shiny.setInputValue(
        "configure-inputEditor-prRemoveLastItem", 
        parts[1], 
        {priority: "event"}
    );
}
Shiny.addCustomMessageHandler('prDuplicateLastInput', function(data) { // execute the add/remove list actions
    let input = $('#' + data.id);
    if(input.length === 0){ // checkboxes
        input = $("input[name='" + data.id + "']");
        let clone = input.clone().attr('name', data.newId);
        clone.insertAfter(input.parent().parent()).wrap('<div class="checkbox"></div>').wrap('<label></label>');
    } else { // text/number inputs
        let attr = 'data-shinyjs-resettable-id';
        input.clone().attr('id', data.newId).attr(attr, data.newId).insertAfter(input);
    }
});
Shiny.addCustomMessageHandler('prRemoveLastInput', function(id) {
    let input = $('#' + id);
    if(input.length === 0){ // checkboxes
        input = $("input[name='" + id + "']");
        input.parent().parent().remove();
    } else { // text/number inputs
        input.remove();
    } 
});

/*  ------------------------------------------------------------------------
    command terminal emulator
    ------------------------------------------------------------------------*/
let commandTerminalHistory = {
    commands: [""],
    offset: 0,
    current: ""
}
let traverseCommandHistory = function(prefix, increment){
    let nCommands = commandTerminalHistory.commands.length;
    if(nCommands <= 1) return;
    let i = commandTerminalHistory.offset + increment;
    if(i < 0 || i > nCommands - 1) return;
    let input = $("#" + prefix + "command");
    if(i === 0 && increment === -1) {
        input.val(commandTerminalHistory.current);
        commandTerminalHistory.offset = i;
        return;
    }
    if(i === 1 && increment === 1) commandTerminalHistory.current = input.val();
    input.val(commandTerminalHistory.commands[i - 1]);
    commandTerminalHistory.offset = i;
    Shiny.setInputValue(prefix + "command", input.val()); // otherwise input$command does not stay current
}
let addCommandToHistory = function(prefix, command){ // executed by call from R when command finishes execution
    let input = $("#" + prefix + "command");
    command = command === "" ? input.val() : command;
    if(command === "") return;
    if(command !== commandTerminalHistory.commands[0]) commandTerminalHistory.commands.unshift(command);
    commandTerminalHistory.offset = 0;
    commandTerminalHistory.current = "";
    input.val("");
}
let activateCommandTerminalKeys = function(prefix){ // executed once when terminal dialog is opened
    let input = $("#" + prefix + "command");
    input.on("keyup", function(e) {
        if(e.keyCode === 13) {
            Shiny.setInputValue(prefix + "command", input.val()); // see comment above
            Shiny.setInputValue(prefix + "commandEnterKey", Math.random(), {priority: "event"});
        }
        else if(e.keyCode === 38) traverseCommandHistory(prefix,  1);
        else if(e.keyCode === 40) traverseCommandHistory(prefix, -1);
    });
}
let scrollCommandTerminalResults = function(prefix){ // keep the results view part at the bottom
    let elem = document.getElementById(prefix + 'results');
    elem.scrollTop = elem.scrollHeight;
    $("#" + prefix + "command").focus();
}

/*  ------------------------------------------------------------------------
    inter-process communcation (IPC) from mdi-apps-framework to the optional mdi-apps-launcher wrapper
    ------------------------------------------------------------------------*/
Shiny.addCustomMessageHandler('frameworkToElectron', function(message) {
    if(window.mdiElectron && window.mdiElectron.frameworkToElectron) 
        window.mdiElectron.frameworkToElectron(message.type, message.data);
});
