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
        var jwt = $('#jwt').val();
        var ws = createWebSocket('/' + jwt);

        ws.onopen = function() {
            ws.send('Hi!');
        };

        ws.onmessage = onMessage;

        $('#join').append('Connecting...');

        return false;
    });
});
