<template>
  <div
  :style="{ width: this.width + 'px', height: this.height + 'px' }">
    <b-carousel
      v-bind:id="id"
      v-model="slide"
      :interval="4000"
      controls
      indicators
      fade
      background="#ababab"
      :img-width="this.width + 'px'"
      :img-height="this.height + 'px'"
      style="text-shadow: 1px 1px 2px #333;"
      @sliding-start="onSlideStart"
      @sliding-end="onSlideEnd"
    >
      <b-carousel-slide
        :text='photo[slide].description'
        v-for="item in this.photo" :key="item.url"
        :background="'#596e8b'"
        >
        <template v-slot:img>
          <img
            class="d-block img-fluid"
            :src="item.url"
            alt="image slot"
            :style="`
              max-width: ${width}px !important;
              max-height: ${height}px !important;
              margin-left: ${(width - photoSize.width) / 2}px !important;
              margin-top: ${(height - photoSize.height) / 2}px !important;
              `"
            v-on:load="onLoadPhoto($event)"
          />
        </template>
      </b-carousel-slide>
    </b-carousel>
  </div>
</template>

<script>
export default {
  name: 'Slideshow',
  props: {
    id: String,
    width: String,
    height: String,
    photo: Array,
  },
  data() {
    return {
      slide: 0,
      sliding: null,
      photoSize: {
        width: 0,
        height: 0,
      },
    };
  },
  computed: {
    photoStyle() {
      return {
        'margin-left': '50px',
        'max-width': '400px',
        'max-height': '600px',
      };
    },
  },
  methods: {
    onLoadPhoto(event) {
      if (event.target.clientWidth > 0 && event.target.clientHeight > 0) {
        this.photoSize.width = event.target.clientWidth;
        this.photoSize.height = event.target.clientHeight;
      }
    },
    onSlideStart() {
      this.sliding = true;
    },
    onSlideEnd() {
      this.sliding = false;
    },
  },
};
</script>

<style lang="scss" scoped>
.slide-photo {
  margin-left: 50px;
  max-width: 400px;
  max-height: 600px;
}
</style>
