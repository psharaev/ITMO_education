<template>
  <div>
    <ViewPost :post="post"
              :cntComment="numberOfComments(post)"
              :user="findUser(post)"
              :key="post.id"/>

    <div class="comment" v-for="comment in findComments(post)" :key="comment.id">
      {{findUser(comment).name}}: {{comment.text}}
    </div>
  </div>
</template>

<script>
import ViewPost from "./ViewPost";

export default {
  name: "Post",
  components: {ViewPost},
  props: ["post", "comments", "users"],
  methods: {
    findComments:function (post) {
      return Object.values(this.comments).filter(comment => comment.postId === post.id)
    },
    numberOfComments: function (post) {
      return Object.values(this.comments).filter(comment => comment.postId === post.id).length
    },
    findUser: function (item) {
      return Object.values(this.users).find((user) => user.id === item.userId)
    },

  },
}
</script>

<style scoped>

</style>