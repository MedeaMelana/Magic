function MagicCtrl($scope) {

  $scope.messages = [];

  $scope.sendMessage = function() {
    $scope.socket.send($scope.message);
    $scope.messages.push('> ' + $scope.message);
    $scope.message = '';
  };

  $scope.socket = new WebSocket("ws://localhost:8080");

  $scope.socket.onmessage = function(event) {
    $scope.$apply(function () {
      $scope.messages.push('< ' + event.data);
    });
  };

  $scope.socket.onerror = function(event) {
    $scope.$apply(function () {
      $scope.messages.push('Error: ' + event.data);
    });
  };

  $scope.socket.onopen = function(event) {
    $scope.$apply(function () {
      $scope.messages.push('CONNECTED');
    });
  };

  $scope.socket.onclose = function(event) {
    $scope.$apply(function () {
      $scope.messages.push('DISCONNECTED');
    });
  };

};
