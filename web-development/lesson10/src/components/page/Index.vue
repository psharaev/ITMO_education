<template>
  <div>
    <ViewPost v-for="post in sortedPosts"
              :post="post"
              :cntComment="numberOfComments(post)"
              :user="findUser(post)"
              :key="post.id"/>
  </div>
</template>

<script>
import ViewPost from "./ViewPost";

export default {
  name: "Index",
  components: {ViewPost},
  props: ["users", "posts", "comments"],
  methods: {
    numberOfComments: function (post) {
      return Object.values(this.comments).filter(comment => comment.postId === post.id).length
    },
    findUser: function (post) {
      return Object.values(this.users).find((user) => user.id === post.userId)
    },

  },
  computed: {
    sortedPosts: function () {
      return Object.values(this.posts).sort((a, b) => b.id - a.id);
    }
  },

}
</script>

<style scoped>

</style>
