import 'rxjs/add/operator/do'
import 'rxjs/add/operator/map'
import 'rxjs/add/operator/catch'
import 'rxjs/add/observable/throw'

import { Observable } from 'rxjs/Observable'
import { Injectable } from '@angular/core'
import { HttpClient, HttpErrorResponse } from '@angular/common/http'

import { IMessage } from '../common/message.interface'

@Injectable()
export class MessageService {
  private _url = 'api/messages.json'

  constructor(private _http: HttpClient) {}

  getMessages(): Observable<IMessage[]> {
    return this._http.get<IMessage[]>(this._url)
      .do(data => console.log(data))
      .catch(this.handleError)
  }

  getMessage(id: string): Observable<IMessage> {
    return this.getMessages()
      .map((messages: IMessage[]) => messages.find(m => m.id === id))
  }

  private handleError(err: HttpErrorResponse) {
    console.log(err.message)
    return Observable.throw(err.message)
  }
}
