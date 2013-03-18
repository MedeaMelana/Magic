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
        var message = JSON.parse(event.data);
        console.log(message);
        if (message.type === 'logEvents') {
          $scope.world = message.world;
        }
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

  $scope.describeActiveStep = function() {
    if ($scope.world && $scope.world.activeStep) {
      var activeStep = $scope.world.activeStep;
      if (activeStep.step) {
        return activeStep.step + ' step (' + activeStep.phase + ' phase)';
      } else {
        return activeStep.phase + ' phase';
      }
    }
  };

};
