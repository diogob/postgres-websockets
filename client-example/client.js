function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':3000' + path;

    return new WebSocket(uri);
}

function onMessage(event) {
    var p = $(document.createElement('p')).text(event.data);

    $('#messages').append(p);
    $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});
}

$(document).ready(function () {
    $('#join-form').submit(function () {
        $('#warnings').html('');
        var user = $('#user').val();
        var ws = createWebSocket('/');

        ws.onopen = function() {
            ws.send('Hi! I am ' + user);
        };

        ws.onmessage = onMessage;

        $('#join').append('Connecting...');

        return false;
    });
});
