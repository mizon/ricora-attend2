<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <link rel="stylesheet" type="text/css" href="${doc-root}css/style.css"/>
    <title><page-title/></title>
  </head>
  <body>
    <div id="container">
      <notice/>
      <h1><page-title/></h1>
      <div class="section">
        <header-comment/>
      </div>
      <div class="section">
        <h2>現在の参加者</h2>
        <table id="attendees">
          <tr><th>Id</th><th>名前</th><th>コメント</th></tr>
          <attendees/>
        </table>
      </div>
      <div id="forms">
        <div class="section">
          <h2>参加登録</h2>
          <form method="post" action="${script-path}new" class="form-new">
            <dl>
              <label><dt>名前</dt><dd><input name="attendee-name" type="text"/></dd></label>
              <label><dt>パスワード</dt><dd><input name="attendee-password" type="password"/></dd></label>
              <label><dt>ひとこと</dt><dd><input name="attendee-comment" type="text"/></dd></label>
            </dl>
            <button type="submit">送信</button>
          </form>
        </div>
        <div class="section">
          <h2>キャンセル</h2>
          <form method="post" action="${script-path}delete">
            <label>Id <input name="attendee-id" type="text"/></label>
            <label>パスワード <input name="attendee-password" type="text"/></label>
            <button type="submit">送信</button>
          </form>
        </div>
      </div>
    </div>
  </body>
</html>
