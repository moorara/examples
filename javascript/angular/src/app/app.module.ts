import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { BrowserModule } from '@angular/platform-browser'
import { FormsModule } from '@angular/forms'
import { HttpClientModule } from '@angular/common/http'

import { AppComponent } from './app.component'
import { HomeComponent } from './home/home.component'
import { MessageComponent } from './message/message.component'
import { MessageListComponent } from './message/message-list.component'

@NgModule({
  imports: [
    BrowserModule,
    FormsModule,
    HttpClientModule,
    RouterModule.forRoot([
      { path: 'home', component: HomeComponent },
      { path: 'messages', component: MessageListComponent },
      { path: 'messages/:id', component: MessageComponent },
      { path: '', pathMatch: 'full', redirectTo: 'home' },
      { path: '**', pathMatch: 'full', redirectTo: 'home' },
    ])
  ],
  providers: [],
  declarations: [
    AppComponent,
    HomeComponent,
    MessageComponent,
    MessageListComponent
  ],
  bootstrap: [ AppComponent ]
})
export class AppModule {}
