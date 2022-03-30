(function() {
  var CarrierWaveCropper,
    bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };

  jQuery(function() {
    return new CarrierWaveCropper();
  });

  CarrierWaveCropper = (function() {
    function CarrierWaveCropper() {
      this.updatePreview = bind(this.updatePreview, this);
      this.update = bind(this.update, this);
      $('#person_avatar_cropbox').Jcrop({
        aspectRatio: 1,
        setSelect: [0, 0, 200, 200],
        onSelect: this.update,
        onChange: this.update
      });
    }

    CarrierWaveCropper.prototype.update = function(coords) {
      $('#person_avatar_crop_x').val(coords.x);
      $('#person_avatar_crop_y').val(coords.y);
      $('#person_avatar_crop_w').val(coords.w);
      $('#person_avatar_crop_h').val(coords.h);
      return this.updatePreview(coords);
    };

    CarrierWaveCropper.prototype.updatePreview = function(coords) {
      return $('#person_avatar_previewbox').css({
        width: Math.round(100 / coords.w * $('#person_avatar_cropbox').width()) + 'px',
        height: Math.round(100 / coords.h * $('#person_avatar_cropbox').height()) + 'px',
        marginLeft: '-' + Math.round(100 / coords.w * coords.x) + 'px',
        marginTop: '-' + Math.round(100 / coords.h * coords.y) + 'px'
      });
    };

    return CarrierWaveCropper;

  })();

}).call(this);
