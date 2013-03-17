function MagicCtrl($scope) {

  $scope.messages = [];

  $scope.sendMessage = function() {
    $scope.socket.send($scope.message);
    $scope.message = '';
  };

  $scope.socket = new WebSocket("ws://localhost:8080");

  $scope.socket.onmessage = function(event) {
    $scope.$apply(function () {
      try {
        console.log(JSON.parse(event.data));
      } catch (e) {
        console.log(event.data);
      }
    });
  };

  $scope.socket.onerror = function(event) {
    $scope.$apply(function () {
      console.log('Error: ' + event.data);
    });
  };

  $scope.socket.onopen = function(event) {
    $scope.$apply(function () {
      console.log('CONNECTED');
    });
  };

  $scope.socket.onclose = function(event) {
    $scope.$apply(function () {
      console.log('DISCONNECTED');
    });
  };

};
