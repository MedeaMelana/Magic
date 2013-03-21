angular.module('magicApp',['ui']);

function MagicCtrl($scope) {

  $scope.sendMessage = function() {
    $scope.socket.send($scope.message);
    $scope.message = '';
  };

  $scope.answer = function(a) {
    console.log("SEND " + a);
    $scope.socket.send(a);
  };

  $scope.socket = new WebSocket("ws://localhost:8080");

  $scope.socket.onmessage = function(event) {
    $scope.$apply(function () {
      try {
        var message = JSON.parse(event.data);
        console.log(message);
        if (message.type === 'logEvents') {
          $scope.world = message.world;
        } else if (message.type === 'askQuestion') {
          $scope.world = message.world;
          $scope.question = message.question;
          $scope.playerIdToAnswer = message.playerId;
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

  $scope.isCardClickable = function(objectRef) {
    return $scope.getClickCardAction(objectRef) != null;
  };

  $scope.isObjectRefEqual = function(objectRef1, objectRef2) {
    return $scope.isZoneRefEqual(objectRef1.zone, objectRef2.zone) &&
      objectRef1.objectId == objectRef2.objectId;
  };

  $scope.isZoneRefEqual = function(zoneRef1, zoneRef2) {
    return zoneRef1.name === zoneRef2.name &&
      zoneRef1.playerId === zoneRef2.playerId;
  };

  $scope.clickCard = function(objectRef) {
    var action = $scope.getClickCardAction(objectRef);
    if (action) {
      action();
    }
  };

  $scope.getClickCardAction = function(objectRef) {
    if ($scope.question.type === 'priorityAction') {
      var i = $scope.indexOf($scope.question.options, function(option) {
        return option.type === 'playCard' &&
          $scope.isObjectRefEqual(option.objectRef, objectRef);
      });
      if (i >= 0) {
        return function() { $scope.answer(i); };
      } else {
        return null;
      }
    }
  };

  $scope.indexOf = function(list, iterator) {
    for (var i = 0; i < list.length; i++) {
      var el = list[i];
      if (iterator(el))
        return i;
    }
    return -1;
  };

};
