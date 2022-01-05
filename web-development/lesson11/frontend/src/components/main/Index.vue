<template>
  <div>
    <article v-for="post in viewPosts" :key="post.id">
      <div class="title">
        <a href="#" @click.prevent="showPost(post)">{{ post.title }}</a>
      </div>
      <div class="information">By {{ post.user.login }}</div>
      <div class="body">{{ post.text }}</div>
      <div class="footer">
        <div class="left">
          <img src="../../assets/img/voteup.png" alt="Vote Up" title="Vote Up"/>
          <span class="positive-score">+173</span>
          <img src="../../assets/img/votedown.png" alt="Vote Down" title="Vote Down"/>
        </div>
        <div class="right">
          <img src="../../assets/img/date_16x16.png" alt="Publish Time" title="Publish Time"/>
          {{ post.creationTime | formatDate}}
          <img src="../../assets/img/comments_16x16.png" class="comments" alt="Comments" title="Comments"/>
          <a href="#">{{ commentsSizes[post.id] }}</a>
        </div>
      </div>
    </article>
  </div>
</template>

<script>
import moment from 'moment';
import Vue from "vue";

Vue.filter('formatDate', function (value) {
  if (value) {
    return moment(String(value)).format('MM/DD/YYYY hh:mm')
  }
});

export default {
  name: "Index",
  props: ["posts", "users", "commentsSizes"],
  computed: {
    viewPosts: function () {
      this.$root.$emit("countCommentsSizes");
      return Object.values(this.posts).sort((a, b) => b.id - a.id);
    }
  },
  methods: {
    showPost: function (post) {
      this.$root.$emit("setPost", post)
      this.$root.$emit("onChangePage", 'Post');
    }
  }
}
</script>

<style scoped>

</style>