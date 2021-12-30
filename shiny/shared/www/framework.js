/*  shiny.js defines client-side functions */

/*  ------------------------------------------------------------------------
    dynamically resize content wrapper to allow it to carry the scrollbar (i.e. header stays fixed)
    ------------------------------------------------------------------------*/
$(document).ready(function() {
    let setCWHeight = function(){
        $(".content-wrapper").height($(window).height() - 200);
    };
    setCWHeight();
    $(window).resize(setCWHeight);
    $(".main-header .logo").on('click', function(){
        Shiny.setInputValue('resetPage', true, {priority: "event"}); 
    });
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
    help Shiny show a spinner on certain slow actions
    ------------------------------------------------------------------------*/
Shiny.addCustomMessageHandler('toggleSpinner', function(visibility) {
    $(".progress-spinner-div").css('visibility',visibility);
});

/*  ------------------------------------------------------------------------
    handle Ace Code Editor
    ------------------------------------------------------------------------*/
Shiny.addCustomMessageHandler('initializeAceCodeEditor', function(editorId) {
    window[editorId] = ace.edit(editorId);    
    window[editorId].setTheme("ace/theme/crimson_editor");
    window[editorId].session.setMode("ace/mode/r");
});
Shiny.addCustomMessageHandler('getAceCodeContents', function(options) {
    let code = window[options.editorId].getValue();
    Shiny.setInputValue(options.editorId + "-contents", 
                        {file: options.file, code: code, flag: options.flag}, 
                        {priority: "event"});
});
Shiny.addCustomMessageHandler('setAceCodeContents', function(options) {
    window[options.editorId].session.setValue(options.code);
});

/*  ------------------------------------------------------------------------
    handle Summernote Editor
    ------------------------------------------------------------------------*/
Shiny.addCustomMessageHandler('getSummernoteCodeContents', function(editorId) {
    let code = $("#" + editorId).summernote('code');
    Shiny.setInputValue(editorId + "-contents", code, {priority: "event"});
});
Shiny.addCustomMessageHandler('setSummernoteCodeContents', function(options) {
    $("#" + options.editorId).summernote('code', options.code);
});

/*  ------------------------------------------------------------------------
    DT table action links
    ------------------------------------------------------------------------*/
let handleActionClick = function(parentId, instanceId, confirmMessage){
    if(confirmMessage === "NO_CONFIRM" || confirm(confirmMessage) === true){
        Shiny.setInputValue(parentId, instanceId + '__' + Math.floor(Math.random() * 1e6)); // random allows repeat clicks
    }
};

/*  ------------------------------------------------------------------------
    Pipeline Runner, functions to simplify the number of required input observers in R
    ------------------------------------------------------------------------*/
let prInputOnChange = function(x){
    let parts = x.id.split('__');
    Shiny.setInputValue(
        parts[0], 
        {id: parts[1], value: x.value, logical: false}, 
        {priority: "event"}
    );
}
let prCheckboxOnChange = function(x){
    let parts = x.name.split('__');
    Shiny.setInputValue(
        parts[0], 
        {id: parts[1], value: $(x).prop("checked"), logical: true}, 
        {priority: "event"}
    );
}
